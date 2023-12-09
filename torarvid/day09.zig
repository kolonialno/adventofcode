const std = @import("std");
const util = @import("util.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var lines = try util.file_as_strings("inputs/day09.txt", gpa.allocator());

    var sum1: isize = 0;
    var sum2: isize = 0;
    for (lines) |line| {
        var nums = try util.line_as_numbers(isize, line, gpa.allocator());
        var deltas = std.ArrayList([]const isize).init(gpa.allocator());
        try deltas.append(nums);
        var depth: usize = 0;
        var all_deltas_zero = false;
        while (!all_deltas_zero) : (depth += 1) {
            var new_deltas = std.ArrayList(isize).init(gpa.allocator());
            all_deltas_zero = true;
            const depth_deltas = deltas.items[depth];
            for (1..depth_deltas.len) |i| {
                const new_delta = depth_deltas[i] - depth_deltas[i - 1];
                if (new_delta != 0) all_deltas_zero = false;
                try new_deltas.append(new_delta);
            }
            try deltas.append(try new_deltas.toOwnedSlice());
        }
        var next: isize = 0;
        var prev: isize = 0;
        while (true) : (depth -= 1) {
            const d = deltas.items[depth];
            next += d[d.len - 1];
            prev = d[0] - prev;
            if (depth == 0) break;
        }
        sum1 += next;
        sum2 += prev;
    }
    std.debug.print("Part 1: {}\nPart 2: {}\n", .{ sum1, sum2 });
}
