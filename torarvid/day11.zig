const std = @import("std");
const util = @import("util.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var map = try util.file_as_strings("inputs/day11.txt", gpa.allocator());

    const sum1 = try run(map, 1, gpa.allocator());
    std.debug.print("Part 1: {}\n", .{sum1});

    const sum2 = try run(map, 999999, gpa.allocator());
    std.debug.print("Part 2: {}\n", .{sum2});
}

fn run(map: []const []const u8, scaling: usize, allocator: std.mem.Allocator) !usize {
    var extra_rows = try extra(map, is_row_empty, scaling, allocator);
    var extra_cols = try extra(map, is_col_empty, scaling, allocator);

    var galaxies = std.ArrayList([2]usize).init(allocator);
    for (map, 0..) |row, y| {
        for (row, 0..) |c, x| {
            if (c == '#') {
                try galaxies.append([2]usize{ @intCast(x), @intCast(y) });
            }
        }
    }

    var sum: usize = 0;
    while (galaxies.items.len > 1) {
        const galaxy = galaxies.pop();
        for (galaxies.items) |other| {
            const max_x = @max(other[0], galaxy[0]);
            const min_x = @min(other[0], galaxy[0]);
            const max_y = @max(other[1], galaxy[1]);
            const min_y = @min(other[1], galaxy[1]);

            var extra_y: usize = 0;
            for (extra_rows) |row| {
                if (row[0] >= min_y and row[0] < max_y) {
                    extra_y += row[1];
                }
            }

            var extra_x: usize = 0;
            for (extra_cols) |col| {
                if (col[0] >= min_x and col[0] < max_x) {
                    extra_x += col[1];
                }
            }

            sum += (max_x - min_x + extra_x) + (max_y - min_y + extra_y);
        }
    }

    return sum;
}

fn extra(
    map: []const []const u8,
    empty_fn: *const fn ([]const []const u8, usize) bool,
    scaling: usize,
    allocator: std.mem.Allocator,
) ![][2]usize {
    var list = std.ArrayList([2]usize).init(allocator);
    var i: usize = map.len - 1;
    while (true) {
        if (empty_fn(map, i)) {
            try list.append([2]usize{ i, scaling });
        }
        if (i == 0) break;
        i -= 1;
    }

    return list.toOwnedSlice();
}

fn is_row_empty(map: []const []const u8, row: usize) bool {
    for (map[row]) |c| {
        if (c != '.') return false;
    }
    return true;
}

fn is_col_empty(map: []const []const u8, col: usize) bool {
    for (map) |row| {
        if (row[col] != '.') return false;
    }
    return true;
}
