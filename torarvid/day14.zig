const std = @import("std");
const util = @import("util.zig");

pub fn main() !void {
    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    var gpa = alloc.allocator();
    var lines = try util.file_as_strings("inputs/day14.txt", gpa);
    // lines = try util.raw_as_strings(test_input, gpa);

    var map = try util.strings_to_mut(lines, gpa);

    north(map);

    std.debug.print("Part 1: {}\n", .{calc_load(map)});

    map = try util.strings_to_mut(lines, gpa);

    for (0..1000000000) |i| {
        north(map);
        west(map);
        south(map);
        east(map);
        if (i % 100000 == 0) {
            std.debug.print("\ri = {}", .{i});
        }
    }
    std.debug.print("Part 2: {}\n", .{calc_load(map)});
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

fn print(map: []const []u8) void {
    for (map) |line| {
        std.debug.print("{s}\n", .{line});
    }
    std.debug.print("\n", .{});
}

const test_input =
    \\O....#....
    \\O.OO#....#
    \\.....##...
    \\OO.#O....O
    \\.O.....O#.
    \\O.#..O.#.#
    \\..O..#O..O
    \\.......O..
    \\#....###..
    \\#OO..#....
;
