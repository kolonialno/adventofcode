const std = @import("std");
const util = @import("util.zig");

pub fn main() !void {
    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = alloc.allocator();
    var lines = try util.file_as_strings("inputs/day13.txt", gpa);

    var start_of_map: usize = 0;
    var sum: usize = 0;
    var reflection_cache = std.AutoHashMap(usize, usize).init(gpa);
    var maps = std.ArrayList([]const []const u8).init(gpa);
    for (lines, 0..) |line, i| {
        if (line.len == 0) {
            const reflection = try findReflection(lines[start_of_map..i], gpa);
            try maps.append(lines[start_of_map..i]);
            try reflection_cache.put(maps.items.len, reflection);
            sum += reflection;
            start_of_map = i + 1;
        }
    }
    const reflection = try findReflection(lines[start_of_map..lines.len], gpa);
    try maps.append(lines[start_of_map..lines.len]);
    try reflection_cache.put(maps.items.len, reflection);
    sum += reflection;

    std.debug.print("Part 1: {}\n", .{sum});

    sum = 0;
    for (maps.items, 1..) |map, i| {
        const cached_reflection = reflection_cache.get(i);
        if (cached_reflection == null) {
            return error.Unreachable;
        }
        var it = SmudgeIterator{ .orig = map, .allocator = gpa };
        var found = false;

        while (try it.next()) |smudged| {
            const new_reflection = try findReflectionExcept(smudged, cached_reflection, gpa);
            if (new_reflection != 0 and new_reflection != cached_reflection) {
                sum += new_reflection;
                found = true;
                break;
            }
        }
        if (!found) {
            std.debug.print("no reflection found for map {d}\n", .{i});
            sum += cached_reflection.?;
        }
    }

    std.debug.print("Part 2: {}\n", .{sum});
}

fn findReflection(map: []const []const u8, allocator: std.mem.Allocator) !usize {
    return findReflectionExcept(map, null, allocator);
}

fn findReflectionExcept(map: []const []const u8, exclude: ?usize, allocator: std.mem.Allocator) !usize {
    for (1..map.len) |i| {
        var ai: usize = i - 1;
        var bi: usize = i;
        var is_reflection = true;
        while (true) {
            const a = map[ai];
            const b = map[bi];
            if (!std.mem.eql(u8, a, b)) {
                is_reflection = false;
                break;
            }
            if (ai < 1 or bi >= map.len - 1) {
                break;
            }
            ai -= 1;
            bi += 1;
        }

        if (is_reflection and exclude != 100 * i) {
            return 100 * i;
        }
    }

    for (1..map[0].len) |i| {
        var ai: usize = i - 1;
        var bi: usize = i;
        var is_reflection = true;
        while (true) {
            const a = try util.col_as_str(map, ai, allocator);
            const b = try util.col_as_str(map, bi, allocator);
            if (!std.mem.eql(u8, a, b)) {
                is_reflection = false;
                break;
            }
            if (ai < 1 or bi >= map[0].len - 1) {
                break;
            }
            ai -= 1;
            bi += 1;
        }
        if (is_reflection and exclude != i) {
            return i;
        }
    }
    return 0;
}

const SmudgeIterator = struct {
    orig: []const []const u8,
    x: usize = 0,
    y: usize = 0,
    allocator: std.mem.Allocator,

    pub fn next(self: *SmudgeIterator) !?[]const []const u8 {
        if (self.y >= self.orig.len) {
            return null;
        }

        var new = try util.strings_to_mut(self.orig, self.allocator);
        if (self.orig[self.y][self.x] == '#') {
            new[self.y][self.x] = '.';
        } else {
            new[self.y][self.x] = '#';
        }

        self.x += 1;
        if (self.x == self.orig[0].len) {
            self.x = 0;
            self.y += 1;
        }

        return new;
    }
};
