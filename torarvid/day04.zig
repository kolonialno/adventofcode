const std = @import("std");
const util = @import("util.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const lines = try util.file_as_strings("inputs/day04.txt", gpa.allocator());

    var sum: u64 = 0;
    for (lines) |card| {
        var iter = std.mem.tokenizeAny(u8, card, ":|");
        _ = iter.next(); // skip "Card <\d+>:"

        const winning_raw = iter.next().?;
        const have_raw = iter.next().?;

        var winning = std.AutoHashMap(u32, void).init(gpa.allocator());
        iter = std.mem.tokenizeAny(u8, winning_raw, " ");
        while (iter.next()) |num_raw| {
            const num = try std.fmt.parseInt(u32, num_raw, 10);
            try winning.put(num, {});
        }

        iter = std.mem.tokenizeAny(u8, have_raw, " ");
        var points: ?u32 = null;
        while (iter.next()) |num_raw| {
            const num = try std.fmt.parseInt(u32, num_raw, 10);
            if (winning.get(num) != null) {
                points = if (points) |p| p * 2 else 1;
            }
        }
        if (points) |p| sum += p;
    }
    std.debug.print("Part 1: {d}\n", .{sum});

    sum = 0;
    var counts = std.AutoHashMap(u32, u64).init(gpa.allocator());
    for (lines) |card| {
        var iter = std.mem.tokenizeAny(u8, card, ":|");
        const card_id_raw = std.mem.trimLeft(u8, iter.next().?[5..], " ");
        const card_id = try std.fmt.parseInt(u32, card_id_raw, 10);

        const winning_raw = iter.next().?;
        const have_raw = iter.next().?;

        var winning = std.AutoHashMap(u32, void).init(gpa.allocator());
        iter = std.mem.tokenizeAny(u8, winning_raw, " ");
        while (iter.next()) |num_raw| {
            const num = try std.fmt.parseInt(u32, num_raw, 10);
            try winning.put(num, {});
        }

        iter = std.mem.tokenizeAny(u8, have_raw, " ");
        var points: ?u32 = null;
        while (iter.next()) |num_raw| {
            const num = try std.fmt.parseInt(u32, num_raw, 10);
            if (winning.get(num) != null) {
                points = (points orelse 0) + 1;
            }
        }
        const entry = try counts.getOrPutValue(card_id, 0);
        entry.value_ptr.* += 1;

        if (points) |p| {
            for (card_id + 1..card_id + 1 + p) |id| {
                const dest_entry = try counts.getOrPutValue(@intCast(id), 0);
                const value_to_add = counts.get(card_id);
                dest_entry.value_ptr.* += value_to_add.?;
            }
        }
    }
    var it = counts.iterator();
    while (it.next()) |e| {
        sum += e.value_ptr.*;
    }

    std.debug.print("Part 2: {d}\n", .{sum});
}
