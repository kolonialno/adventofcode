const std = @import("std");
const Allocator = std.mem.Allocator;

fn conv_string(comptime T: type, in: []const u8, allocator: Allocator) ![]const u8 {
    _ = T;
    return try std.mem.Allocator.dupe(allocator, u8, in);
}

pub fn file_as_strings(filename: []const u8, allocator: Allocator) ![]const []const u8 {
    var buffer: [1024 * 1024]u8 = undefined;
    return file_as_whatever(filename, allocator, &buffer, []const u8, conv_string);
}

pub fn read_as_strings(reader: *std.Io.Reader, allocator: Allocator) ![]const []const u8 {
    return read_as_whatever(reader, allocator, []const u8, conv_string);
}

fn conv_number(comptime T: type, in: []const u8, allocator: Allocator) std.fmt.ParseIntError!T {
    _ = allocator;
    return try std.fmt.parseInt(T, in, 10);
}

pub fn file_as_numbers(filename: []const u8, allocator: Allocator) ![]const i32 {
    return file_as_whatever(filename, allocator, i32, conv_number);
}

// Opens a file, reads it line by line, and converts each line to a T.
pub fn file_as_whatever(
    filename: []const u8,
    allocator: Allocator,
    buffer: []u8,
    comptime T: type,
    conv_fn: fn (comptime T: type, in: []const u8, allocator: Allocator) anyerror!T,
) ![]const T {
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();
    var readwrap = file.reader(buffer);
    const reader = &readwrap.interface;

    return read_as_whatever(reader, allocator, T, conv_fn);
}

pub fn read_as_whatever(
    reader: *std.Io.Reader,
    allocator: Allocator,
    comptime T: type,
    comptime conv_fn: fn (comptime T: type, in: []const u8, allocator: Allocator) anyerror!T,
) ![]const T {
    var list = std.ArrayList(T){};

    while (reader.takeDelimiterExclusive('\n')) |line| {
        const element = try conv_fn(T, line, allocator);
        try list.append(allocator, element);
        _ = reader.peek(1) catch |err| switch (err) {
            error.EndOfStream => {
                break;
            },
            else => return err,
        };
        reader.toss(1);
    } else |err| switch (err) {
        error.EndOfStream => {},
        else => return err,
    }

    const slice = list.toOwnedSlice(allocator);
    list.clearAndFree(allocator);
    return slice;
}

// Splits a string into lines. Useful for the test inputs.
pub fn raw_as_strings(in: []const u8, allocator: Allocator) ![]const []const u8 {
    var it = std.mem.splitScalar(u8, in, '\n');
    var list = std.ArrayList([]const u8){};
    while (it.next()) |line| {
        try list.append(allocator, line);
    }
    return list.toOwnedSlice(allocator);
}

pub fn line_as_numbers(comptime T: type, line: []const u8, allocator: std.mem.Allocator) ![]const T {
    var it = std.mem.splitScalar(u8, line, ' ');
    var list = std.ArrayList(T){};
    while (it.next()) |number| {
        const element = try std.fmt.parseInt(T, number, 10);
        try list.append(allocator, element);
    }
    return list.toOwnedSlice(allocator);
}

pub fn str_repeat(src: []const u8, copies: usize, allocator: std.mem.Allocator) ![]const u8 {
    var list = std.ArrayList(u8){};
    for (copies) |_| {
        try list.appendSlice(allocator, src);
    }
    return try list.toOwnedSlice(allocator);
}

pub fn str_splice(src: []const u8, split_point: usize, insert: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    var list = std.ArrayList(u8){};
    try list.appendSlice(allocator, src[0..split_point]);
    try list.appendSlice(allocator, insert);
    try list.appendSlice(allocator, src[split_point..]);
    return try list.toOwnedSlice(allocator);
}

pub fn col_as_str(input: []const []const u8, col: usize, allocator: std.mem.Allocator) ![]const u8 {
    var list = std.ArrayList(u8){};
    for (input) |line| {
        try list.append(allocator, line[col]);
    }
    return try list.toOwnedSlice(allocator);
}

pub fn strings_to_mut(in: []const []const u8, allocator: std.mem.Allocator) ![]const []u8 {
    var list = std.ArrayList([]u8){};
    for (in) |line| {
        try list.append(allocator, try std.mem.Allocator.dupe(allocator, u8, line));
    }
    return try list.toOwnedSlice(allocator);
}
