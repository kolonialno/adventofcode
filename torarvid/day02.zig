const std = @import("std");
const Allocator = std.mem.Allocator;
const util = @import("util.zig");

const Range = struct {
    start: u64,
    end: u64,
};

pub fn part1(ranges: std.ArrayList(Range)) !void {
    var sum: u64 = 0;
    for (ranges.items) |range| {
        var buf: [100]u8 = undefined;
        for (range.start..range.end + 1) |num| {
            const str = std.fmt.bufPrint(&buf, "{d}", .{num}) catch unreachable;
            if (str.len % 2 == 1) {
                continue;
            }
            if (std.mem.eql(u8, str[0 .. str.len / 2], str[str.len / 2 .. str.len])) {
                sum += num;
            }
        }
    }
    std.debug.print("Part 1: {d}\n", .{sum});
}

pub fn part2(ranges: std.ArrayList(Range)) !void {
    var sum: u64 = 0;
    for (ranges.items) |range| {
        var buf: [100]u8 = undefined;
        for (range.start..range.end + 1) |num| {
            const str = std.fmt.bufPrint(&buf, "{d}", .{num}) catch unreachable;
            for (1..1 + (str.len / 2)) |len| {
                if (str.len % len != 0) {
                    continue;
                }
                const initial = str[0..len];
                const num_chunks: u32 = @intCast(str.len / len);
                var valid = true;
                for (1..num_chunks) |offset| {
                    const cmp = str[offset * len .. offset * len + len];
                    if (!std.mem.eql(u8, initial, cmp)) {
                        valid = false;
                        break;
                    }
                }
                if (valid) {
                    sum += num;
                    break;
                }
            }
        }
    }
    std.debug.print("Part 2: {d}\n", .{sum});
}

pub fn run(input: []const []const u8, allocator: Allocator) !void {
    var iter = std.mem.splitAny(u8, input[0], ",-");
    var ranges = std.ArrayList(Range){};
    while (true) {
        const first = iter.next() orelse break;
        const second = iter.next() orelse unreachable;
        const start = try std.fmt.parseInt(u64, first, 10);
        const end = try std.fmt.parseInt(u64, second, 10);
        try ranges.append(allocator, Range{ .start = start, .end = end });
    }
    try part1(ranges);
    try part2(ranges);
    ranges.clearAndFree(allocator);
    for (input) |line| {
        allocator.free(line);
    }
    allocator.free(input);
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const input = try util.file_as_strings("inputs/day02.txt", allocator);

    try run(input, allocator);
}

test "day02" {
    const allocator = std.testing.allocator;
    const raw =
        \\11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124
    ;
    var reader = std.Io.Reader.fixed(raw);

    const input = try util.read_as_strings(&reader, allocator);
    try run(input, allocator);
}
