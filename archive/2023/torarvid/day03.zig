const std = @import("std");
const util = @import("util.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const lines = try util.file_as_strings("inputs/day03.txt", gpa.allocator());

    var hash = std.StringHashMap(*u32).init(gpa.allocator());

    for (lines, 0..) |line, y| {
        var num: ?*u32 = null;
        for (line, 0..) |c, x| {
            if (c >= '0' and c <= '9') {
                if (num) |n| {
                    n.* = n.* * 10 + (c - '0');
                } else {
                    const n = try gpa.allocator().create(u32);
                    n.* = c - '0';
                    num = n;
                }
                const key = try std.fmt.allocPrint(gpa.allocator(), "{d},{d}", .{ y, x });
                if (num) |n| {
                    const nn: *u32 = n;
                    try hash.put(key, nn);
                }
            } else {
                num = null;
            }
        }
    }

    var included_numbers = std.AutoHashMap(*u32, void).init(gpa.allocator());
    for (lines, 0..) |line, y| {
        for (line, 0..) |c, x| {
            if (c != '.' and (c < '0' or c > '9')) {
                for (y - 1..y + 2) |yy| {
                    for (x - 1..x + 2) |xx| {
                        if (yy < 0 or xx < 0 or yy == lines.len or xx == line.len) {
                            continue;
                        }
                        if (yy == y and xx == x) {
                            continue;
                        }
                        const key = try std.fmt.allocPrint(gpa.allocator(), "{d},{d}", .{ yy, xx });
                        if (hash.get(key)) |n| {
                            try included_numbers.put(n, {});
                        }
                    }
                }
            }
        }
    }
    var i = included_numbers.keyIterator();
    var sum: u32 = 0;
    while (i.next()) |e| {
        sum += e.*.*;
    }
    std.debug.print("Part 1: {d}\n", .{sum});

    var part2_sum: u32 = 0;
    for (lines, 0..) |line, y| {
        for (line, 0..) |c, x| {
            if (c == '*') {
                included_numbers = std.AutoHashMap(*u32, void).init(gpa.allocator());
                for (y - 1..y + 2) |yy| {
                    for (x - 1..x + 2) |xx| {
                        if (yy < 0 or xx < 0 or yy == lines.len or xx == line.len) {
                            continue;
                        }
                        if (yy == y and xx == x) {
                            continue;
                        }
                        const key = try std.fmt.allocPrint(gpa.allocator(), "{d},{d}", .{ yy, xx });
                        if (hash.get(key)) |n| {
                            try included_numbers.put(n, {});
                        }
                    }
                }
                var count: u32 = 0;
                var it = included_numbers.iterator();
                var product: u32 = 1;
                while (it.next()) |e| {
                    count += 1;
                    product *= e.key_ptr.*.*;
                }
                if (count == 2) {
                    part2_sum += product;
                }
            }
        }
    }
    std.debug.print("Part 2: {d}\n", .{part2_sum});
}
