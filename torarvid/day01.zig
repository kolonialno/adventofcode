const std = @import("std");
const Allocator = std.mem.Allocator;
const util = @import("util.zig");

pub fn part1(input: []const []const u8) !void {
    var password: u16 = 0;
    var position: i9 = 50;
    for (input) |line| {
        var adjustment: i12 = 1;
        if (line[0] == 'L') {
            adjustment = -1;
        }
        const element = try std.fmt.parseInt(u11, line[1..line.len], 10);
        adjustment *= element;
        const new_position = @mod(position + adjustment, 100);
        position = @intCast(new_position);
        if (position == 0) {
            password += 1;
        }
    }

    std.debug.print("Part 1: {d}\n", .{password});
}

pub fn part2(input: []const []const u8) !void {
    var password: u16 = 0;
    var position: i9 = 50;
    for (input) |line| {
        var adjustment: i12 = 1;
        if (line[0] == 'L') {
            adjustment = -1;
        }
        const element = try std.fmt.parseInt(u11, line[1..line.len], 10);
        for (0..element) |_| {
            const new_position = @mod(position + adjustment, 100);
            position = @intCast(new_position);
            if (position == 0) {
                password += 1;
            }
        }
    }

    std.debug.print("Part 2: {d}\n", .{password});
}

pub fn run(input: []const []const u8, allocator: Allocator) !void {
    try part1(input);
    try part2(input);

    for (input) |line| {
        allocator.free(line);
    }
    allocator.free(input);
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const input = try util.file_as_strings("inputs/day01.txt", allocator);

    try run(input, allocator);
}

test "day01" {
    const allocator = std.testing.allocator;
    const raw =
        \\L68
        \\L30
        \\R48
        \\L5
        \\R60
        \\L55
        \\L1
        \\L99
        \\R14
        \\L82
    ;
    var reader = std.Io.Reader.fixed(raw);

    const input = try util.read_as_strings(&reader, allocator);
    try run(input, allocator);
}
