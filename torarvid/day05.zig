const std = @import("std");
const util = @import("util.zig");

const Range = struct {
    start: u64,
    end: u64,

    pub fn combine(self: Range, other: Range) struct { data: [2]Range, len: usize } {
        if (self.start <= other.start and self.end >= other.end) {
            return .{ .data = [_]Range{ .{ .start = self.start, .end = self.end }, undefined }, .len = 1 };
        } else if (self.start >= other.start and self.end <= other.end) {
            return .{ .data = [_]Range{ .{ .start = other.start, .end = other.end }, undefined }, .len = 1 };
        } else if (self.start <= other.start and self.end + 1 >= other.start) {
            return .{ .data = [_]Range{ .{ .start = self.start, .end = other.end }, undefined }, .len = 1 };
        } else if (self.start <= other.end + 1 and self.end >= other.end) {
            return .{ .data = [_]Range{ .{ .start = other.start, .end = self.end }, undefined }, .len = 1 };
        }

        return .{ .data = [_]Range{ .{ .start = self.start, .end = self.end }, .{ .start = other.start, .end = other.end } }, .len = 2 };
    }
};

fn p1(ranges: []const Range, avail_raw: []const []u8) u64 {
    var count: u64 = 0;
    for (avail_raw) |raw| {
        const avail = std.fmt.parseInt(u64, raw, 10) catch unreachable;
        for (ranges) |range| {
            if (avail >= range.start and avail <= range.end) {
                count += 1;
                break;
            }
        }
    }
    return count;
}

fn p2(ranges: *std.ArrayList(Range)) u64 {
    var count: u64 = 0;
    while (true) {
        const old_len = ranges.items.len;
        for (0..ranges.items.len - 1) |j| {
            for (1..ranges.items.len) |i| {
                if (i >= ranges.items.len or j >= ranges.items.len) {
                    break;
                }
                if (i == j) {
                    continue;
                }
                const cmb = ranges.items[j].combine(ranges.items[i]);
                ranges.items[j] = cmb.data[0];
                if (cmb.len == 1) {
                    _ = ranges.orderedRemove(i);
                } else {
                    ranges.items[i] = cmb.data[1];
                }
            }
        }
        if (old_len == ranges.items.len) {
            break;
        }
    }
    for (ranges.items) |range| {
        count += range.end - range.start + 1;
    }
    return count;
}

pub fn run(input: []const []u8, allocator: std.mem.Allocator) !void {
    defer {
        for (input) |line| {
            allocator.free(line);
        }
        allocator.free(input);
    }
    var buf: [1024]Range = undefined;
    var buf2: [1024][]u8 = undefined;
    var ranges = std.ArrayList(Range).initBuffer(&buf);
    var lines = std.ArrayList([]u8).initBuffer(&buf2);
    var parse_range = true;
    for (input) |line| {
        if (std.mem.eql(u8, line, "")) {
            parse_range = false;
            continue;
        }
        if (parse_range) {
            var split = std.mem.splitScalar(u8, line, '-');
            const start = std.fmt.parseInt(u64, split.next() orelse unreachable, 10) catch unreachable;
            const end = std.fmt.parseInt(u64, split.next() orelse unreachable, 10) catch unreachable;
            ranges.appendBounded(Range{ .start = start, .end = end }) catch unreachable;
        } else {
            try lines.appendBounded(line);
        }
    }

    const part1 = p1(ranges.items, lines.items);
    std.debug.print("Part 1: {d}\n", .{part1});

    const part2 = p2(&ranges);
    std.debug.print("Part 2: {d}\n", .{part2});
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const input = try util.file_as_strings("inputs/day05.txt", allocator);
    try run(input, allocator);
}

test "day02" {
    const raw =
        \\3-5
        \\10-14
        \\16-20
        \\12-18
        \\
        \\1
        \\5
        \\8
        \\11
        \\17
        \\32
    ;
    var reader = std.Io.Reader.fixed(raw);

    const allocator = std.testing.allocator;
    const input = try util.read_as_strings(&reader, allocator);
    try run(input, allocator);
}
