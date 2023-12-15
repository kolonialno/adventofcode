const std = @import("std");
const util = @import("util.zig");

pub fn main() !void {
    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    var gpa = alloc.allocator();
    var lines = try util.file_as_strings("inputs/day15.txt", gpa);

    var it = std.mem.splitAny(u8, lines[0], ",");

    var sum: usize = 0;
    while (it.next()) |token| {
        sum += hash(token);
    }
    std.debug.print("Part 1: {}\n", .{sum});

    var boxes: [256]*std.ArrayList(Box) = undefined;
    for (0..256) |i| {
        var list = try gpa.create(std.ArrayList(Box));
        list.* = std.ArrayList(Box).init(gpa);
        boxes[i] = list;
    }

    it = std.mem.splitAny(u8, lines[0], ",");
    while (it.next()) |op| {
        var op_it = std.mem.tokenizeAny(u8, op, "-=");
        const label = op_it.next().?;
        const focal = op_it.next();
        const label_hash = hash(label);
        var list = boxes[label_hash];

        const existing_index = for (0..list.items.len) |i| {
            if (std.mem.eql(u8, list.items[i].label, label)) break i;
        } else null;

        if (focal) |f| {
            var focal_int = try std.fmt.parseInt(usize, f, 10);
            if (existing_index) |i| {
                list.items[i].focal = focal_int;
            } else {
                try list.append(Box{ .label = label, .focal = focal_int });
            }
        } else if (existing_index) |i| {
            _ = list.orderedRemove(i);
        }
    }

    sum = 0;
    for (0..256) |i| {
        var list = boxes[i];
        for (0..list.items.len) |j| {
            const bn = 1 + i;
            const slot = 1 + j;
            const foc = list.items[j].focal;
            sum += bn * slot * foc;
        }
    }

    std.debug.print("Part 2: {}\n", .{sum});
}

const Box = struct {
    label: []const u8,
    focal: usize,
};

fn hash(string: []const u8) u8 {
    var h: usize = 0;
    for (string) |c| {
        h += c;
        h *= 17;
        h %= 256;
    }
    return @intCast(h);
}
