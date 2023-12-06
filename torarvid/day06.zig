const std = @import("std");
const util = @import("util.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var lines = try util.file_as_strings("inputs/day06.txt", gpa.allocator());

    const times = try parse_1(lines[0][9..], gpa.allocator());
    const distances = try parse_1(lines[1][9..], gpa.allocator());

    var answer: usize = 1;
    for (0..times.items.len) |i| {
        const time = times.items[i];
        const record_distance = distances.items[i];

        var local_ways_to_win: usize = 0;
        for (0..time + 1) |t| {
            const distance = (time - t) * t;
            if (distance > record_distance) {
                local_ways_to_win += 1;
            }
        }

        answer *= local_ways_to_win;
    }

    std.debug.print("Part 1: {}\n", .{answer});

    const time = try parse_2(lines[0][9..], gpa.allocator());
    const record_distance = try parse_2(lines[1][9..], gpa.allocator());

    var ways_to_win: usize = 0;
    for (0..time + 1) |t| {
        const distance = (time - t) * t;
        if (distance > record_distance) {
            ways_to_win += 1;
        }
    }

    std.debug.print("Part 2: {}\n", .{ways_to_win});
}

fn parse_1(input: []const u8, allocator: std.mem.Allocator) !std.ArrayList(usize) {
    var it = std.mem.tokenizeAny(u8, input, " ");
    var list = std.ArrayList(usize).init(allocator);
    while (it.next()) |token| {
        try list.append(try std.fmt.parseInt(usize, token, 10));
    }

    return list;
}

fn parse_2(input: []const u8, allocator: std.mem.Allocator) !usize {
    var it = std.mem.tokenizeAny(u8, input, " ");
    var s: []const u8 = "";
    while (it.next()) |token| {
        const slices: []const []const u8 = &[_][]const u8{ s, token };
        s = try std.mem.concat(allocator, u8, slices);
    }

    return try std.fmt.parseInt(usize, s, 10);
}
