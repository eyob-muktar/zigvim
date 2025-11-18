const std = @import("std");
const debug = std.debug;
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const os = std.posix;
const math = std.math;
const Allocator = mem.Allocator;

const EDITOR_VERSION: []const u8 = "0.0.1";
const TAB_STOP: u8 = 4;

// Cursor key constants
const BACKSPACE: u16 = 127;
const ARROW_UP: u16 = 1000;
const ARROW_DOWN: u16 = 1001;
const ARROW_LEFT: u16 = 1002;
const ARROW_RIGHT: u16 = 1003;
const PAGE_UP: u16 = 1004;
const PAGE_DOWN: u16 = 1005;
const HOME_KEY: u16 = 1006;
const END_KEY: u16 = 1007;
const DEL_KEY: u16 = 1008;

const HIGHLIGHT = enum(u8) { HL_NORMAL = 0, HL_NUMBER, HL_STRING, HL_MATCH, HL_COMMENT };

pub fn promptDefaultArgFn(editor: *Editor, buffer: []const u8, char: u16) !void {
    _ = buffer[0..];
    _ = char;
    _ = editor.cursor_y;
    return;
}

pub fn syntaxToColor(highlight: HIGHLIGHT) u8 {
    switch (highlight) {
        .HL_COMMENT => return 36,
        .HL_STRING => return 35,
        .HL_NUMBER => return 31,
        .HL_MATCH => return 34,
        else => return 37,
    }
}

pub fn isSeparator(char: u8) bool {
    const is_separator = if (std.mem.indexOfScalar(u8, &[_]u8{ ',', '.', '(', ')', '+', '-', '/', '*', '=', '~', '%', '<', '>', '[', ']', ';' }, char)) |_| true else false;
    return std.ascii.isWhitespace(char) or is_separator;
}

// const HLDB = [_]EditorSyntax{.{ .file_type = &[_]u8{'c'}, .file_extensions = &[_][]const u8{ ".c", ".h", ".cpp" }, .flags = .{ .HL_NUMBER = true, .HL_STRING = true }, .single_line_comment = &[_]u8{"ab"} }};
const HLDB = [_]EditorSyntax{.{ .file_type = "c", .file_extensions = &[_][]const u8{ ".c", ".h", ".cpp" }, .flags = .{ .HL_NUMBER = true, .HL_STRING = true }, .single_line_comment = "//" }};

const EditorSyntax = struct { file_type: []const u8, file_extensions: []const []const u8, flags: packed struct {
    HL_NUMBER: bool = false,
    HL_STRING: bool = false,
    HL_MLCOMMENT: bool = false,
    HL_COMMENT: bool = false,
    HL_KEYWORD1: bool = false,
    HL_KEYWORD2: bool = false,
}, single_line_comment: ?[]const u8 = null };
const PromptArgs = struct { addn_msg: []const u8 = "", callback: fn (e: *Editor, buffer: []const u8, char: u16) anyerror!void = promptDefaultArgFn };

const EditorRow = struct {
    chars: std.ArrayList(u8),
    size: u16,
    render: std.ArrayList(u8),
    highlight: std.ArrayList(HIGHLIGHT),
    editor: *Editor,

    const Self = @This();

    pub fn init(alloc: std.mem.Allocator, editor: *Editor) !EditorRow {
        const chars = try std.ArrayList(u8).initCapacity(alloc, 100);
        const render = try std.ArrayList(u8).initCapacity(alloc, 100);
        const highlight = try std.ArrayList(HIGHLIGHT).initCapacity(alloc, 100);
        return EditorRow{ .chars = chars, .render = render, .size = 0, .highlight = highlight, .editor = editor };
    }

    pub fn deinit(self: *Self) void {
        self.chars.deinit();
        self.render.deinit();
    }

    pub fn insertChar(self: *Self, at: u16, char: u8) !void {
        if (at < 0 or at > self.size) {
            try self.chars.insert(self.size, char);
        } else try self.chars.insert(at, char);

        self.size += 1;
        try self.update();
    }

    pub fn deleteChar(self: *Self, at: u16) !void {
        if (at < 0 or at >= self.size) return;
        _ = self.chars.orderedRemove(at);
        self.size -= 1;
        try self.update();
    }

    pub fn appendChars(self: *Self, chars: []u8, size: u16) !void {
        try self.chars.appendSlice(chars);
        self.size += size;
        try self.update();
    }

    pub fn removeChars(self: *Self, at: u16) !void {
        self.chars.shrinkAndFree(at);
        self.size = at;
        try self.update();
    }

    pub fn update(self: *Self) !void {
        var idx: u8 = 0;
        self.render.clearAndFree();
        for (self.chars.items) |char| {
            if (char == '\t') {
                idx += 1;
                try self.render.append(' ');
                while (idx % TAB_STOP != 0) : (idx += 1) try self.render.append(' ');
                continue;
            }
            try self.render.append(char);
        }
        try self.updateSyntax();
    }

    pub fn updateSyntax(self: *Self) !void {
        self.highlight.clearAndFree();
        try self.highlight.appendNTimes(HIGHLIGHT.HL_NORMAL, self.render.items.len);

        var prev_sep = true;
        var in_string: u8 = 0;

        if (self.editor.file_type) |syntax| {
            const slc: ?[]const u8 = syntax.single_line_comment;
            var slc_len: u8 = 0;
            if (slc) |value| {
                slc_len = @intCast(value.len);
            }

            var i: u8 = 0;
            while (i < self.render.items.len) : (i += 1) {
                const char = self.render.items[i];
                const prev_hl = if (i > 0) self.highlight.items[i - 1] else HIGHLIGHT.HL_NORMAL;

                if (slc_len > 0 and in_string == 0) {
                    if (std.mem.startsWith(u8, self.render.items[i..], slc.?)) {
                        @memset(self.highlight.items[i..self.highlight.items.len], HIGHLIGHT.HL_COMMENT);
                        break;
                    }
                }

                if (syntax.flags.HL_STRING) {
                    if (in_string != 0) {
                        try self.highlight.insert(i, HIGHLIGHT.HL_STRING);
                        if (char == '\\' and i + 1 < self.render.items.len) {
                            i += 1;
                            try self.highlight.insert(i, HIGHLIGHT.HL_STRING);
                        }
                        if (char == in_string) {
                            in_string = 0;
                        }
                        prev_sep = true;
                        continue;
                    } else if (char == '"' or char == '\'') {
                        in_string = char;
                        try self.highlight.insert(i, HIGHLIGHT.HL_STRING);
                        continue;
                    }
                }

                if (syntax.flags.HL_NUMBER) {
                    if ((std.ascii.isDigit(char) and (prev_sep or prev_hl == HIGHLIGHT.HL_NUMBER)) or (char == '.' and prev_hl == HIGHLIGHT.HL_NUMBER)) {
                        try self.highlight.insert(i, HIGHLIGHT.HL_NUMBER);
                        prev_sep = false;
                        continue;
                    }
                }
                prev_sep = isSeparator(char);
            }
        }
    }
};

const Editor = struct {
    tty: fs.File,
    og_termios: os.termios,
    screen_row: u16,
    screen_col: u16,
    content_buffer: std.ArrayList(u8),
    cursor_x: u16,
    cursor_y: u16,
    rows: std.ArrayList(EditorRow),
    num_of_lines: u16,
    alloc: std.mem.Allocator,
    row_offset: u16,
    col_offset: u16,
    rendered_cx: u16, // for tabs
    filename: []u8,
    status_msg: std.ArrayList(u8),
    time_t: i64,
    dirty: u8,
    file_type: ?EditorSyntax,

    const Self = @This();

    // Terminal functions
    pub fn init(tty: fs.File, termios: os.termios, allocator: std.mem.Allocator) !Editor {
        var winsize: os.winsize = undefined;
        _ = os.system.ioctl(tty.handle, os.T.IOCGWINSZ, @intFromPtr(&winsize));
        const content_buffer = try std.ArrayList(u8).initCapacity(allocator, 100);
        const status_msg = try std.ArrayList(u8).initCapacity(allocator, 100);
        const rows = try std.ArrayList(EditorRow).initCapacity(allocator, 100);

        return Editor{
            .tty = tty,
            .og_termios = termios,
            .screen_row = winsize.row - 2,
            .screen_col = winsize.col,
            .content_buffer = content_buffer,
            .cursor_x = 0,
            .cursor_y = 0,
            // .line = line_buffer,
            .rows = rows,
            .num_of_lines = 0,
            .alloc = allocator,
            .row_offset = 0,
            .col_offset = 0,
            .rendered_cx = 0,
            .filename = "",
            .status_msg = status_msg,
            .time_t = 0,
            .dirty = 0,
            .file_type = null,
        };
    }

    pub fn deinit(self: *Self) void {
        self.content_buffer.deinit();
        self.rows.deinit();
        self.status_msg.deinit();
        self.alloc.destroy();
    }

    pub fn enableRawMode(self: Self) !void {
        var raw_termios = self.og_termios;

        raw_termios.lflag.ECHO = false; // stop printing back inputs
        raw_termios.lflag.ICANON = false; // canonical mode (input read line by line)
        raw_termios.lflag.ISIG = false; // interupting signals (ctrl-z, ctrl-c)
        raw_termios.lflag.IEXTEN = false; // ctrl-v, ctrl-c

        raw_termios.iflag.IXON = false; // software flow signals ctrl-s
        raw_termios.iflag.IXOFF = false; // software flow signals ctrl-q
        raw_termios.iflag.ICRNL = false; // carriage return new char (ctrl-m)

        raw_termios.oflag.OPOST = false;

        raw_termios.cc[@intFromEnum(os.V.MIN)] = 1;
        raw_termios.cc[@intFromEnum(os.V.TIME)] = 0;

        try os.tcsetattr(self.tty.handle, .FLUSH, raw_termios);
    }

    fn disableRawMode(self: Self) !void {
        try os.tcsetattr(self.tty.handle, .FLUSH, self.og_termios);
    }

    // Editor operations
    fn readKey(self: *Self) !u16 {
        var buffer: [4]u8 = undefined;
        _ = try self.tty.read(&buffer);

        if (buffer[0] == '\x1b') {
            if (buffer[1] == '[') {
                if (buffer[2] >= '0' and buffer[2] <= '9') {
                    if (buffer[3] == '~') {
                        switch (buffer[2]) {
                            '1' => return HOME_KEY,
                            '3' => return DEL_KEY,
                            '4' => return END_KEY,
                            '5' => return PAGE_UP,
                            '6' => return PAGE_DOWN,
                            '7' => return HOME_KEY,
                            '8' => return END_KEY,
                            else => return '\x1b',
                        }
                    }
                }
                switch (buffer[2]) {
                    'A' => return ARROW_UP,
                    'B' => return ARROW_DOWN,
                    'C' => return ARROW_RIGHT,
                    'D' => return ARROW_LEFT,
                    'H' => return HOME_KEY,
                    'F' => return END_KEY,
                    else => return '\x1b',
                }
            } else if (buffer[1] == 'O') {
                switch (buffer[2]) {
                    'H' => return HOME_KEY,
                    'F' => return END_KEY,
                    else => return '\x1b',
                }
            }
            return '\x1b';
        }
        return @as(u16, buffer[0]);
    }

    fn processKeyPress(self: *Self) !void {
        const char = try self.readKey();
        switch (char) {
            '\r' => {
                try self.insertNewLine();
            },
            CTRL('q') => {
                try self.exit();
            },
            CTRL('s') => {
                try self.save();
            },
            HOME_KEY => {
                self.cursor_x = 0;
            },
            END_KEY => {
                if (self.cursor_y < self.num_of_lines) {
                    // const row = self.rows.items[self.cursor_y].chars.items;
                    // self.cursor_x = @truncate(row.len);
                    self.cursor_x = self.rows.items[self.cursor_y].size;
                }
            },
            CTRL('f') => {
                try self.find();
            },
            BACKSPACE, CTRL('h'), DEL_KEY => {
                if (char == DEL_KEY) self.moveCursor(ARROW_RIGHT);
                try self.deleteChar();
            },
            ARROW_UP, ARROW_DOWN, ARROW_LEFT, ARROW_RIGHT => {
                self.moveCursor(char);
            },
            PAGE_UP => {
                self.cursor_y = self.row_offset;
                for (0..self.screen_row) |_| {
                    self.moveCursor(ARROW_UP);
                }
            },
            PAGE_DOWN => {
                self.cursor_y = self.row_offset + self.screen_row - 1;
                if (self.cursor_y > self.num_of_lines) self.cursor_y = self.num_of_lines;
                for (0..self.screen_row) |_| {
                    self.moveCursor(ARROW_DOWN);
                }
            },
            CTRL('l'), '\x1b' => {},
            '\n' => {
                debug.print("input {}", .{char});
            },
            else => {
                try self.insertChar(@intCast(char));
            },
        }
    }

    fn refreshScreen(self: *Self) !void {
        self.scroll();

        var appendBuffer: [10]u8 = undefined;
        const cursor_x_str = try std.fmt.bufPrint(&appendBuffer, "\x1b[{d};{d}H", .{ (self.cursor_y - self.row_offset) + 1, (self.rendered_cx - self.col_offset) + 1 }); //move the cursor
        try self.content_buffer.appendSlice("\x1b[?25l"); //clear the screen line by line
        try self.content_buffer.appendSlice("\x1b[H");

        try self.drawRows();
        try self.drawStatusBar();
        try self.drawMessageBar();

        try self.content_buffer.appendSlice(cursor_x_str);
        try self.content_buffer.appendSlice("\x1b[?25h");
        _ = try self.tty.writer().print("{s}", .{self.content_buffer.items});
    }

    fn moveCursor(self: *Self, key: u16) void {
        var row: ?EditorRow = if (self.cursor_y < self.num_of_lines) self.rows.items[self.cursor_y] else null;

        switch (key) {
            ARROW_LEFT => {
                if (self.cursor_x != 0) { //prevent integer overflow
                    self.cursor_x -= 1;
                } else if (self.cursor_y > 0) {
                    self.cursor_y -= 1;
                    // self.cursor_x = @truncate(self.rows.items[self.cursor_y].chars.items.len);
                    self.cursor_x = self.rows.items[self.cursor_y].size;
                }
            },
            ARROW_RIGHT => {
                if (row) |value| {
                    if (self.cursor_x < value.size) {
                        self.cursor_x += 1;
                    } else if (self.cursor_y < self.num_of_lines) {
                        self.cursor_y += 1;
                        self.cursor_x = 0;
                    }
                }
            },
            ARROW_UP => {
                if (self.cursor_y != 0) {
                    self.cursor_y -= 1;
                }
            },
            ARROW_DOWN => {
                if (self.cursor_y < self.num_of_lines) {
                    self.cursor_y += 1;
                }
            },
            else => {
                // do nothing
            },
        }
        row = if (self.cursor_y < self.num_of_lines) self.rows.items[self.cursor_y] else null;
        const row_len = if (row) |value| value.size else 0;
        if (self.cursor_x > row_len) {
            // self.cursor_x = @truncate(row_len);
            self.cursor_x = row_len;
        }
    }

    fn scroll(self: *Self) void {
        self.rendered_cx = 0;
        if (self.cursor_y < self.num_of_lines) {
            self.rendered_cx = rowCxToRenderedCx(self.rows.items[self.cursor_y], self.cursor_x);
        }

        if (self.cursor_y < self.row_offset) {
            self.row_offset = self.cursor_y;
        }
        if (self.cursor_y >= self.row_offset + self.screen_row) {
            self.row_offset = self.cursor_y - self.screen_row + 1;
        }
        if (self.rendered_cx < self.col_offset) {
            self.col_offset = self.rendered_cx;
        }
        if (self.rendered_cx >= self.col_offset + self.screen_col) {
            self.col_offset = self.rendered_cx - self.screen_col + 1;
        }
    }

    fn rowCxToRenderedCx(row: EditorRow, cursor_x: u16) u16 {
        var rendered_cx: u16 = 0;
        for (0..cursor_x) |value| {
            if (row.chars.items[value] == '\t') {
                rendered_cx += (TAB_STOP - 1) - (rendered_cx % TAB_STOP);
            }
            rendered_cx += 1;
        }
        return rendered_cx;
    }

    fn renderedCxToRowCx(row: EditorRow, rendered_cx: u16) u16 {
        var cur_rx: u16 = 0;
        for (row.chars.items, 0..) |char, index| {
            if (char == '\t') {
                cur_rx += (TAB_STOP - 1) - (cur_rx % TAB_STOP);
            }
            cur_rx += 1;

            if (cur_rx > rendered_cx) return @intCast(index);
        }
        return 0;
    }

    fn drawStatusBar(self: *Self) !void {
        try self.content_buffer.appendSlice("\x1b[7m"); //sequence to invert color
        var statusBuffer: [100]u8 = undefined;
        var rowBuffer: [100]u8 = undefined;
        const filename_len = if (self.filename.len < 20) self.filename.len else 20;
        const status = try std.fmt.bufPrint(&statusBuffer, "{s} - {d} lines {s}", .{ if (self.filename.len == 0) "[No Name]" else self.filename[0..filename_len], self.num_of_lines, if (self.dirty == 0) "" else "(modified)" });
        const row_status = try std.fmt.bufPrint(&rowBuffer, "{s} | {d}/{d}", .{ if (self.file_type) |ftype| ftype.file_type else "no type", self.cursor_y + 1, self.cursor_x });
        var len: usize = if (status.len > self.screen_col) self.screen_col else status.len;
        try self.content_buffer.appendSlice(status[0..]);
        while (len < self.screen_col) : (len += 1) {
            if (self.screen_col - len == row_status.len) {
                try self.content_buffer.appendSlice(row_status);
                break;
            } else {
                try self.content_buffer.append(' ');
            }
        }
        try self.content_buffer.appendSlice("\x1b[m"); //sequence to restore color
        try self.content_buffer.appendSlice("\r\n");
    }

    fn setStatusMessage(self: *Self, msg: []const u8) !void {
        self.status_msg.clearAndFree();
        try self.status_msg.appendSlice(msg[0..]);
        self.time_t = std.time.timestamp();
    }

    fn drawMessageBar(self: *Self) !void {
        try self.content_buffer.appendSlice("\x1b[K");
        var msg_len = self.status_msg.items.len;
        if (msg_len > self.screen_col) msg_len = self.screen_col;
        const now = std.time.timestamp();
        if (msg_len > 0 and now - self.time_t < 5) try self.content_buffer.appendSlice(self.status_msg.items[0..]);
    }

    fn exit(self: *Self) !void {
        const static = struct {
            var quit_times: u8 = 3;
        };

        if (self.dirty > 0 and static.quit_times > 0) {
            try self.setStatusMessage("WARNING!!! File has unsaved changes. Press CTRL-Q 3 times to quit.");
            static.quit_times -= 1;
            return;
        }

        try self.disableRawMode();
        _ = try self.tty.write("\x1b[2J");
        _ = try self.tty.write("\x1b[H");
        os.exit(0);
    }

    fn prompt(self: *Self, prompt_msg: []const u8, args: PromptArgs) ![]u8 {
        var chars = try std.ArrayList(u8).initCapacity(self.alloc, 100);
        const zero = 0;
        while (true) {
            try self.setStatusMessage(prompt_msg[0..]);
            try self.status_msg.appendSlice(chars.items);
            try self.status_msg.appendSlice(args.addn_msg);
            try self.refreshScreen();

            const char = try self.readKey();
            if (char == DEL_KEY or char == CTRL('h') or char == BACKSPACE) {
                _ = if (chars.items.len > 0) chars.pop() else null;
            } else if (char == '\x1b') {
                try self.setStatusMessage("");
                try args.callback(self, chars.items[0..], char);
                chars.clearAndFree();
                return "";
            } else if (char == '\r') {
                if (chars.items.len != 0) {
                    try self.setStatusMessage("");
                    try args.callback(self, chars.items[0..], char);
                    return chars.items[zero..];
                }
            } else if (char < 128) {
                try chars.append(@intCast(char));
            }
            try args.callback(self, chars.items[0..], char);
        }
    }

    // Row operations
    fn drawRows(self: *Self) !void {
        for (0..self.screen_row) |value| {
            const file_row = value + self.row_offset;
            if (file_row >= self.num_of_lines) {
                if (self.num_of_lines == 0 and value == self.screen_row / 3) {
                    const title: []const u8 = "Zig Editor -- version " ++ EDITOR_VERSION;
                    var title_len = title.len;
                    if (title_len > self.screen_col) title_len = self.screen_col;
                    var padding = (self.screen_col - title_len) / 2;
                    if (padding > 0) {
                        try self.content_buffer.append('~');
                        padding -= 1;
                    }
                    while (padding > 0) : (padding -= 1) {
                        try self.content_buffer.append(' ');
                    }
                    try self.content_buffer.appendSlice(title);
                } else {
                    try self.content_buffer.append('~');
                }
            } else {
                var len: usize = 0;
                const char_len = self.rows.items[file_row].render.items.len;
                if (char_len > self.col_offset) {
                    len = char_len - self.col_offset;
                }
                if (len > self.screen_col) len = self.screen_col;
                if (char_len >= self.col_offset) {
                    const upper_bound = len + self.col_offset;
                    // try self.content_buffer.appendSlice(self.rows.items[file_row].render.items[self.col_offset..upper_bound]);
                    const row = self.rows.items[file_row];
                    const line = row.render.items[self.col_offset..upper_bound];
                    const highlight = row.highlight.items[self.col_offset..upper_bound];
                    var current_color: u8 = 0;
                    for (line, 0..) |char, index| {
                        if (highlight[index] == HIGHLIGHT.HL_NORMAL) {
                            if (current_color != 0) {
                                try self.content_buffer.appendSlice("\x1b[39m");
                                current_color = 0;
                            }
                            try self.content_buffer.append(char);
                        } else {
                            const color = syntaxToColor(highlight[index]);
                            if (color != current_color) {
                                current_color = color;
                                // var buffer: [16]u8 = undefined;
                                // var str = try std.fmt.bufPrint(&buffer, "\x1b[{d}m", .{color});
                                try self.content_buffer.writer().print("\x1b[{d}m", .{color});
                                // try self.content_buffer.appendSlice(str[0..]);
                            }
                            try self.content_buffer.append(char);
                        }
                    }
                    try self.content_buffer.appendSlice("\x1b[39m");
                }
            }

            try self.content_buffer.appendSlice("\x1b[K");
            try self.content_buffer.appendSlice("\r\n");
        }
    }

    fn insertRow(self: *Self, at: u16, line: []u8) !void {
        var row = try EditorRow.init(self.alloc, self);
        try row.chars.appendSlice(line[0..]);
        row.size += @intCast(line.len);
        try row.update();
        try self.rows.insert(at, row);
        self.num_of_lines += 1;
        self.dirty += 1;
    }

    fn deleteRow(self: *Self, at: u16) !void {
        if (at >= self.num_of_lines) return;
        _ = self.rows.orderedRemove(at);
        self.num_of_lines -= 1;
    }

    fn insertChar(self: *Self, char: u8) !void {
        if (self.cursor_y == self.num_of_lines) {
            try self.insertRow(self.num_of_lines, "");
        }
        var row = &self.rows.items[self.cursor_y];
        try row.insertChar(self.cursor_x, char);
        self.cursor_x += 1;
        self.dirty += 1;
    }

    fn deleteChar(self: *Self) !void {
        if (self.cursor_y == self.num_of_lines) return;
        if (self.cursor_y == 0 and self.cursor_x == 0) return;

        var row = &self.rows.items[self.cursor_y];
        if (self.cursor_x > 0) {
            try row.deleteChar(self.cursor_x - 1);
            self.cursor_x -= 1;
        } else {
            const rowToRemove = self.rows.items[self.cursor_y];
            var rowToAppendTo = &self.rows.items[self.cursor_y - 1];
            self.cursor_x = rowToAppendTo.size;

            try rowToAppendTo.appendChars(rowToRemove.chars.items, rowToRemove.size);
            try self.deleteRow(self.cursor_y);

            self.cursor_y -= 1;
        }
        self.dirty += 1;
    }

    fn insertNewLine(self: *Self) !void {
        if (self.cursor_x == 0) {
            try self.insertRow(self.cursor_y, "");
        } else {
            const row = &self.rows.items[self.cursor_y];
            try self.insertRow(self.cursor_y + 1, row.chars.items[self.cursor_x..]);
            try row.removeChars(self.cursor_x);
        }
        self.cursor_y += 1;
        self.cursor_x = 0;
    }

    // File i/o
    fn open(self: *Self, filename: []u8) !void {
        const file = try fs.cwd().openFile(filename, .{});
        defer file.close();
        self.filename = filename[0..];

        try self.selectSyntaxHighlight();

        var buffered_reader = std.io.bufferedReader(file.reader());
        var in_stream = buffered_reader.reader();

        var buffer: [1024]u8 = undefined;
        @memset(buffer[0..], 0);

        while (try in_stream.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
            try self.insertRow(self.num_of_lines, line);
        }
        self.dirty = 0;
    }

    fn save(self: *Self) !void {
        if (self.filename.len == 0) {
            const filename: []u8 = try self.prompt("Save as: ", .{});
            if (filename.len == 0) {
                try self.setStatusMessage("Save aborted");
                return;
            }
            self.filename = filename;
        }
        try self.selectSyntaxHighlight();

        const file = try fs.cwd().createFile(self.filename, .{});
        defer file.close();

        var buffered_writer = std.io.bufferedWriter(file.writer());
        var out_stream = buffered_writer.writer();

        for (self.rows.items) |row| {
            _ = try out_stream.write(row.chars.items[0..]);
            _ = try out_stream.writeByte('\n');
        }
        try buffered_writer.flush();
        self.dirty = 0;
        try self.setStatusMessage("Saved to disk");
    }

    // Find
    fn find(self: *Self) !void {
        const saved_cx = self.cursor_x;
        const saved_cy = self.cursor_y;
        const saved_col_off = self.col_offset;
        const saved_row_off = self.row_offset;

        const query = try self.prompt("Search: ", .{ .addn_msg = " (ESC to cancel)", .callback = findCallback });

        if (query.len == 0) {
            self.cursor_x = saved_cx;
            self.cursor_y = saved_cy;
            self.col_offset = saved_col_off;
            self.row_offset = saved_row_off;
        }
    }

    fn findCallback(self: *Self, query: []const u8, key: u16) !void {
        const static = struct {
            var last_match: i32 = -1;
            var direction: i8 = 1;
            var saved_hl_line: u8 = 0;
            var saved_hl = std.ArrayList(HIGHLIGHT).initCapacity(self.alloc, 100) catch unreachable;
        };

        if (key == '\r' or key == '\x1b') {
            static.last_match = -1;
            static.direction = 1;
            return;
        } else if (key == ARROW_RIGHT or key == ARROW_DOWN) {
            static.direction = 1;
        } else if (key == ARROW_LEFT or key == ARROW_UP) {
            static.direction = -1;
        } else {
            static.last_match = -1;
            static.direction = 1;
        }
        if (static.last_match == -1) static.direction = 1;
        var current: i32 = static.last_match;

        for (self.rows.items) |row| {
            current += static.direction;
            if (current == -1) {
                current = self.num_of_lines - 1;
            } else if (current == self.num_of_lines) {
                current = 0;
            }
            const items = self.rows.items[@intCast(current)].render.items;

            const match = std.ascii.indexOfIgnoreCase(items, query);
            // std.mem.indexOf(u8, row.render.items, query);
            if (match) |not_null_index| {
                static.last_match = current;
                self.cursor_y = @intCast(current);
                self.cursor_x = renderedCxToRowCx(row, @intCast(not_null_index));
                self.row_offset = self.num_of_lines;

                // static.saved_hl_line = @intCast(current);
                // @memcpy(static.saved_hl, self.rows.items[@intCast(current)].highlight.items[0..]);
                // static.saved_hl = self.rows.items[@intCast(current)].highlight.items[zero..];

                var a = [_]HIGHLIGHT{.HL_MATCH} ** 15;
                try self.rows.items[@intCast(current)].highlight.replaceRange(not_null_index, query.len, a[0..query.len]);
                //         self.rows.items[0].highlight.shrinkAndFree(100);
                // };

                break;
            }
        }
    }

    fn selectSyntaxHighlight(self: *Self) !void {
        self.file_type = null;
        if (self.filename.len == 0) return;
        const last_dot_index = std.mem.lastIndexOfScalar(u8, self.filename, '.');
        if (last_dot_index) |index| {
            const file_ext = self.filename[index..];
            if (index == 0) return;
            for (HLDB) |value| {
                for (value.file_extensions) |ext| {
                    if (std.mem.eql(u8, file_ext, ext)) {
                        self.file_type = value;

                        for (self.rows.items) |*row| {
                            try row.updateSyntax();
                        }
                    }
                }
            }
        } else return;
    }
};

pub fn CTRL(key: u8) u8 {
    return ((key) & 0x1f);
}

pub fn main() !void {
    var tty = try fs.cwd().openFile("/dev/tty", .{ .mode = .read_write });
    defer tty.close();
    const og_termios = try os.tcgetattr(tty.handle);

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var editor = try Editor.init(tty, og_termios, allocator);
    try editor.enableRawMode();

    // open the editor with file if file arg exist
    if (args.len >= 2) {
        const filename = args[1];
        try editor.open(filename);
    }
    const status_msg = "HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find";
    try editor.setStatusMessage(status_msg);
    while (true) {
        try editor.refreshScreen();
        try editor.processKeyPress();
    }
}
