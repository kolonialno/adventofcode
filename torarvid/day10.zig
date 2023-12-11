const std = @import("std");
const util = @import("util.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var lines = try util.file_as_strings("inputs/day10.txt", gpa.allocator());

    var map = try Map.init(lines, gpa.allocator());
    const steps = try map.findFarthestNode();
    std.debug.print("Part 1: {}\n", .{steps});
}

const Map = struct {
    map: []const []const u8,
    sx: usize,
    sy: usize,
    set: std.StringHashMap(void),
    allocator: std.mem.Allocator,

    fn init(map: []const []const u8, allocator: std.mem.Allocator) !Map {
        for (0..map.len) |y| {
            const line = map[y];
            for (0..line.len) |x| {
                if (line[x] == 'S') {
                    var set = std.StringHashMap(void).init(allocator);
                    var m: Map = .{ .map = map, .sx = x, .sy = y, .set = set, .allocator = allocator };
                    try m.mark(x, y);
                    return m;
                }
            }
        }
        return error.InvalidMap;
    }

    fn mark(self: *Map, x: usize, y: usize) !void {
        const key = try std.fmt.allocPrint(self.allocator, "{},{}", .{ x, y });
        try self.set.put(key, {});
    }

    fn findFarthestNode(self: *Map) !usize {
        var start_it = PipeIterator{ .map = self, .curr_x = self.sx, .curr_y = self.sy, .prev_x = 0, .prev_y = 0 };
        const start = start_it.next().?;
        start_it = PipeIterator{ .map = self, .curr_x = self.sx, .curr_y = self.sy, .prev_x = start[0], .prev_y = start[1] };
        const start2 = start_it.next().?;
        var it1 = PipeIterator{ .map = self, .curr_x = start[0], .curr_y = start[1], .prev_x = self.sx, .prev_y = self.sy };
        var it2 = PipeIterator{ .map = self, .curr_x = start2[0], .curr_y = start2[1], .prev_x = self.sx, .prev_y = self.sy };
        var steps: usize = 1;
        while (true) {
            const next1 = it1.next();
            const next2 = it2.next();
            if (next1 == null or next2 == null) {
                break;
            }
            try self.mark(next1.?[0], next1.?[1]);
            try self.mark(next2.?[0], next2.?[1]);
            steps += 1;
            if (next1.?[0] == next2.?[0] and next1.?[1] == next2.?[1]) {
                break;
            }
        }
        var it = self.set.iterator();
        var count: usize = 0;
        while (it.next()) |_| {
            count += 1;
        }
        return steps;
    }
};

const PipeIterator = struct {
    map: *Map,
    curr_x: usize,
    curr_y: usize,
    prev_x: usize,
    prev_y: usize,

    fn next(self: *PipeIterator) ?[2]usize {
        var result: [2]usize = undefined;
        const map = self.map.map;
        const c = map[self.curr_y][self.curr_x];
        if (self.curr_x > 0 and (c == 'S' or c == '-' or c == 'J' or c == '7')) {
            const n = map[self.curr_y][self.curr_x - 1];
            if (n == '-' or n == 'L' or n == 'F') {
                if (self.prev_x != self.curr_x - 1 or self.prev_y != self.curr_y) {
                    result[0] = self.curr_x - 1;
                    result[1] = self.curr_y;
                }
            }
        }
        if (self.curr_x < map[self.curr_y].len - 1 and (c == 'S' or c == '-' or c == 'L' or c == 'F')) {
            const n = map[self.curr_y][self.curr_x + 1];
            if (n == '-' or n == 'J' or n == '7') {
                if (self.prev_x != self.curr_x + 1 or self.prev_y != self.curr_y) {
                    result[0] = self.curr_x + 1;
                    result[1] = self.curr_y;
                }
            }
        }
        if (self.curr_y > 0 and (c == 'S' or c == '|' or c == 'L' or c == 'J')) {
            const n = map[self.curr_y - 1][self.curr_x];
            if (n == '|' or n == '7' or n == 'F') {
                if (self.prev_x != self.curr_x or self.prev_y != self.curr_y - 1) {
                    result[0] = self.curr_x;
                    result[1] = self.curr_y - 1;
                }
            }
        }
        if (self.curr_y < map.len - 1 and (c == 'S' or c == '|' or c == '7' or c == 'F')) {
            const n = map[self.curr_y + 1][self.curr_x];
            if (n == '|' or n == 'J' or n == 'L') {
                if (self.prev_x != self.curr_x or self.prev_y != self.curr_y + 1) {
                    result[0] = self.curr_x;
                    result[1] = self.curr_y + 1;
                }
            }
        }
        self.prev_x = self.curr_x;
        self.prev_y = self.curr_y;
        self.curr_x = result[0];
        self.curr_y = result[1];
        return result;
    }
};
