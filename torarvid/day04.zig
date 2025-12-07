const std = @import("std");
const util = @import("util.zig");

const Grid = struct {
    buf: std.ArrayList([]u8),

    const Self = @This();

    const NeighborIter = struct {
        x: isize,
        y: isize,
        grid: *const Grid,
        x_off: i3 = -2,
        y_off: i3 = -1,

        pub fn next(self: *@This()) ?u8 {
            while (true) {
                if (self.x_off == 1) {
                    self.x_off = -1;
                    self.y_off += 1;
                } else {
                    self.x_off += 1;
                }
                if (self.y_off > 1) {
                    return null;
                }
                if (self.x_off == 0 and self.y_off == 0) {
                    continue;
                }
                const xi = self.x + self.x_off;
                const yi = self.y + self.y_off;
                const items = self.grid.buf.items;
                if (xi < 0 or yi < 0 or xi >= items[0].len or yi >= items.len) {
                    continue;
                }
                return self.grid.buf.items[@intCast(yi)][@intCast(xi)];
            }
        }
    };

    fn neighbors(self: *const Self, x: usize, y: usize) NeighborIter {
        return NeighborIter{
            .x = @intCast(x),
            .y = @intCast(y),
            .grid = self,
        };
    }

    fn include(self: *const Self, x: usize, y: usize) bool {
        if (self.buf.items[y][x] != '@') {
            return false;
        }
        var count: u4 = 0;
        var iter = self.neighbors(x, y);
        while (iter.next()) |c| {
            if (c == '@') {
                count += 1;
            }
        }
        return count < 4;
    }

    pub fn count_rolls(self: *const Self) u64 {
        var count: u64 = 0;
        for (self.buf.items, 0..) |line, y| {
            for (line, 0..) |_, x| {
                if (self.include(x, y)) {
                    count += 1;
                }
            }
        }
        return count;
    }

    pub fn remove(self: *Self) u64 {
        var arr: [10000]usize = undefined;
        var next_offset: usize = 0;
        for (self.buf.items, 0..) |line, y| {
            for (line, 0..) |_, x| {
                if (self.include(x, y)) {
                    arr[next_offset] = x;
                    arr[next_offset + 1] = y;
                    next_offset += 2;
                }
            }
        }
        const count = next_offset / 2;

        while (next_offset > 0) {
            const x = arr[next_offset - 2];
            const y = arr[next_offset - 1];
            self.buf.items[y][x] = '.';
            next_offset -= 2;
        }

        return count;
    }
};

pub fn run(input: []const []u8, allocator: std.mem.Allocator) !void {
    defer {
        for (input) |line| {
            allocator.free(line);
        }
        allocator.free(input);
    }
    var buf: [1024][]u8 = undefined;
    var lines = std.ArrayList([]u8).initBuffer(&buf);
    for (input) |line| {
        try lines.appendBounded(line);
    }
    var grid = Grid{ .buf = lines };

    const part1 = grid.count_rolls();
    std.debug.print("Part 1: {d}\n", .{part1});

    var count: u64 = 0;
    while (true) {
        const removed = grid.remove();
        if (removed == 0) {
            break;
        }
        count += removed;
    }
    const part2 = count;
    std.debug.print("Part 2: {d}\n", .{part2});
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const input = try util.file_as_strings("inputs/day04.txt", allocator);
    try run(input, allocator);
}

test "day02" {
    const raw =
        \\..@@.@@@@.
        \\@@@.@.@.@@
        \\@@@@@.@.@@
        \\@.@@@@..@.
        \\@@.@@@@.@@
        \\.@@@@@@@.@
        \\.@.@.@.@@@
        \\@.@@@.@@@@
        \\.@@@@@@@@.
        \\@.@.@@@.@.
    ;
    var reader = std.Io.Reader.fixed(raw);

    const allocator = std.testing.allocator;
    const input = try util.read_as_strings(&reader, allocator);
    try run(input, allocator);
}
