const std = @import("std");
const util = @import("util.zig");

pub fn main() !void {
    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = alloc.allocator();
    const lines = try util.file_as_strings("inputs/day14.txt", gpa);

    var map = try util.strings_to_mut(lines, gpa);

    north(map);
    std.debug.print("Part 1: {}\n", .{calc_load(map)});

    map = try util.strings_to_mut(lines, gpa);

    var cache = std.StringHashMap(usize).init(gpa);
    var key: []const u8 = undefined;
    const repeats_at = for (0..1000000000) |i| {
        north(map);
        west(map);
        south(map);
        east(map);

        key = try cache_key(map, gpa);
        if (cache.get(key)) |_| {
            break i;
        }
        try cache.put(key, i);
    } else 0;
    const cycle = repeats_at - cache.get(key).?;

    const remaining = 1000000000 - repeats_at - 1;
    const modulated = remaining % cycle;
    for (0..modulated) |_| {
        north(map);
        west(map);
        south(map);
        east(map);
    }
    std.debug.print("Part 2: {}\n", .{calc_load(map)});
}

fn cache_key(map: []const []const u8, allocator: std.mem.Allocator) ![]const u8 {
    var h = std.crypto.hash.Md5.init(.{});
    var out: [16]u8 = undefined;
    for (map) |line| {
        h.update(line);
    }
    h.final(out[0..]);

    const hex = std.fmt.fmtSliceHexLower(out[0..]);
    return try std.fmt.allocPrint(allocator, "{s}", .{hex});
}

fn calc_load(map: []const []const u8) usize {
    var load: usize = 0;
    for (0..map.len) |i| {
        const scale = map.len - i;
        const count = std.mem.count(u8, map[i], "O");
        load += count * scale;
    }

    return load;
}

fn north(map: []const []u8) void {
    for (0..map[0].len) |x| {
        for (1..map.len) |y| {
            var dest = y;
            var line = map[dest];
            if (line[x] == 'O') {
                for (0..y) |_| {
                    const candidate = map[dest - 1][x];
                    if (candidate != '.') {
                        break;
                    }
                    dest -= 1;
                }
                if (dest != y) {
                    map[dest][x] = 'O';
                    line[x] = '.';
                }
            }
        }
    }
}

fn south(map: []const []u8) void {
    for (0..map[0].len) |x| {
        for (1..map.len) |y| {
            var dest = map.len - 1 - y;
            var line = map[dest];
            if (line[x] == 'O') {
                for (0..y) |_| {
                    const candidate = map[dest + 1][x];
                    if (candidate != '.') {
                        break;
                    }
                    dest += 1;
                }
                if (dest != map.len - 1 - y) {
                    map[dest][x] = 'O';
                    line[x] = '.';
                }
            }
        }
    }
}

fn west(map: []const []u8) void {
    for (0..map.len) |y| {
        for (1..map[0].len) |x| {
            var dest = x;
            if (map[y][dest] == 'O') {
                for (0..x) |_| {
                    const candidate = map[y][dest - 1];
                    if (candidate != '.') {
                        break;
                    }
                    dest -= 1;
                }
                if (dest != x) {
                    map[y][dest] = 'O';
                    map[y][x] = '.';
                }
            }
        }
    }
}

fn east(map: []const []u8) void {
    for (0..map.len) |y| {
        for (1..map[0].len) |x| {
            var dest = map[0].len - 1 - x;
            if (map[y][dest] == 'O') {
                for (0..x) |_| {
                    const candidate = map[y][dest + 1];
                    if (candidate != '.') {
                        break;
                    }
                    dest += 1;
                }
                if (dest != map[0].len - 1 - x) {
                    map[y][dest] = 'O';
                    map[y][map[0].len - 1 - x] = '.';
                }
            }
        }
    }
}
