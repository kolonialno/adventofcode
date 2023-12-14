const std = @import("std");
const fs = std.fs;
const Allocator = std.mem.Allocator;

fn conv_string(comptime T: type, in: []const u8, allocator: Allocator) ![]const u8 {
    _ = T;
    return try std.mem.Allocator.dupe(allocator, u8, in);
}

pub fn file_as_strings(filename: []const u8, allocator: Allocator) ![]const []const u8 {
    return file_as_whatever(filename, allocator, []const u8, conv_string);
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
    comptime T: type,
    conv_fn: fn (comptime T: type, in: []const u8, allocator: Allocator) anyerror!T,
) ![]const T {
    const file = try fs.cwd().openFile(filename, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    const in_stream = buf_reader.reader();

    var list = std.ArrayList(T).init(allocator);
    // overwritten on each iteration. conv_fn should copy the data.
    var line_buf: [1024 * 1024]u8 = undefined;

    while (try in_stream.readUntilDelimiterOrEof(&line_buf, '\n')) |line| {
        const element = try conv_fn(T, line, allocator);
        try list.append(element);
    }

    return list.toOwnedSlice();
}

// Splits a string into lines. Useful for the test inputs.
pub fn raw_as_strings(in: []const u8, allocator: Allocator) ![]const []const u8 {
    var it = std.mem.split(u8, in, "\n");
    var list = std.ArrayList([]const u8).init(allocator);
    while (it.next()) |line| {
        try list.append(line);
    }
    return list.toOwnedSlice();
}

pub fn line_as_numbers(comptime T: type, line: []const u8, allocator: std.mem.Allocator) ![]const T {
    var it = std.mem.split(u8, line, " ");
    var list = std.ArrayList(T).init(allocator);
    while (it.next()) |number| {
        const element = try std.fmt.parseInt(T, number, 10);
        try list.append(element);
    }
    return list.toOwnedSlice();
}

pub fn str_repeat(src: []const u8, copies: usize, allocator: std.mem.Allocator) ![]const u8 {
    var list = std.ArrayList(u8).init(allocator);
    for (copies) |_| {
        try list.appendSlice(src);
    }
    return try list.toOwnedSlice();
}

pub fn str_splice(src: []const u8, split_point: usize, insert: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    var list = std.ArrayList(u8).init(allocator);
    try list.appendSlice(src[0..split_point]);
    try list.appendSlice(insert);
    try list.appendSlice(src[split_point..]);
    return try list.toOwnedSlice();
}

pub fn col_as_str(input: []const []const u8, col: usize, allocator: std.mem.Allocator) ![]const u8 {
    var list = std.ArrayList(u8).init(allocator);
    for (input) |line| {
        try list.append(line[col]);
    }
    return try list.toOwnedSlice();
}
