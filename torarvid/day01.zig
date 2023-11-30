const std = @import("std");
const util = @import("util.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var lines = try util.file_as_strings("inputs/day01.txt", gpa.allocator());
    var sum: u32 = 0;

    for (lines) |line| {
        var first: ?u8 = null;
        var last: ?u8 = null;
        for (line, 0..) |c, i| {
            if (c >= '0' and c <= '9' and first == null) {
                first = c - '0';
            }
            const c2 = line[line.len - 1 - i];
            if (c2 >= '0' and c2 <= '9' and last == null) {
                last = c2 - '0';
            }
        }
        sum += (first orelse 0) * 10 + (last orelse 0);
    }
    std.debug.print("Part 1: {d}\n", .{sum});

    sum = 0;
    for (lines) |line| {
        var first: ?u8 = null;
        var last: ?u8 = null;
        var i: u16 = 0;
        while (i < line.len) : (i += 1) {
            if (first == null) {
                if (is_digit(line[i..])) |d| {
                    first = d;
                }
            }
            if (last == null) {
                if (is_digit(line[line.len - 1 - i ..])) |d| {
                    last = d;
                }
            }
        }
        sum += (first orelse 0) * 10 + (last orelse 0);
    }
    std.debug.print("Part 2: {d}\n", .{sum});
}

fn is_digit(slice: []const u8) ?u8 {
    if (slice[0] >= '0' and slice[0] <= '9') {
        return slice[0] - '0';
    }
    const named = [_][]const u8{ "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" };
    for (named, 0..) |n, i| {
        const max = @min(n.len, slice.len);
        if (std.mem.eql(u8, slice[0..max], n)) {
            return @intCast(i);
        }
    }
    return null;
}
