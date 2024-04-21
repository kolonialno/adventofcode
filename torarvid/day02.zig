const std = @import("std");
const util = @import("util.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const lines = try util.file_as_strings("inputs/day02.txt", gpa.allocator());
    // lines = try util.raw_as_strings(test_input, gpa.allocator());

    const max_r = 12;
    const max_g = 13;
    const max_b = 14;
    var game_id_sum: u16 = 0;

    for (lines) |game| {
        var it = std.mem.splitSequence(u8, game, ":");
        const fallback: []const u8 = "Fallback";
        const game_id: []const u8 = (it.next() orelse fallback)[5..];
        const sets: []const u8 = (it.next() orelse fallback);
        it = std.mem.splitSequence(u8, sets, ";");
        var possible = true;
        while (it.next()) |set| {
            var set_it = std.mem.splitSequence(u8, set, ",");
            var r: u32 = 0;
            var g: u32 = 0;
            var b: u32 = 0;
            while (set_it.next()) |item| {
                const trimmed = std.mem.trim(u8, item, " ");
                var item_it = std.mem.split(u8, trimmed, " ");
                const num = try std.fmt.parseInt(u32, item_it.next() orelse fallback, 10);
                const color = item_it.next() orelse fallback;
                if (std.mem.eql(u8, color, "red")) {
                    r = num;
                } else if (std.mem.eql(u8, color, "green")) {
                    g = num;
                } else if (std.mem.eql(u8, color, "blue")) {
                    b = num;
                }
            }
            if (r > max_r or g > max_g or b > max_b) {
                possible = false;
            }
        }
        if (possible) {
            game_id_sum += try std.fmt.parseInt(u7, game_id, 10);
        }
    }
    std.debug.print("Part 1: {d}\n", .{game_id_sum});

    var power_sum: u32 = 0;
    for (lines) |game| {
        var it = std.mem.splitSequence(u8, game, ":");
        const fallback: []const u8 = "Fallback";
        const game_id: []const u8 = (it.next() orelse fallback)[5..];
        _ = game_id;
        const sets: []const u8 = (it.next() orelse fallback);
        it = std.mem.splitSequence(u8, sets, ";");
        var min_r: u32 = 0;
        var min_g: u32 = 0;
        var min_b: u32 = 0;
        while (it.next()) |set| {
            var set_it = std.mem.splitSequence(u8, set, ",");
            var r: u32 = 0;
            var g: u32 = 0;
            var b: u32 = 0;
            while (set_it.next()) |item| {
                const trimmed = std.mem.trim(u8, item, " ");
                var item_it = std.mem.split(u8, trimmed, " ");
                const num = try std.fmt.parseInt(u32, item_it.next() orelse fallback, 10);
                const color = item_it.next() orelse fallback;
                if (std.mem.eql(u8, color, "red")) {
                    r = if (num > r) num else r;
                } else if (std.mem.eql(u8, color, "green")) {
                    g = if (num > g) num else g;
                } else if (std.mem.eql(u8, color, "blue")) {
                    b = if (num > b) num else b;
                }
            }
            min_r = if (r > min_r and r > 0) r else min_r;
            min_g = if (g > min_g and g > 0) g else min_g;
            min_b = if (b > min_b and b > 0) b else min_b;
        }
        const power = min_r * min_g * min_b;
        power_sum += power;
    }
    std.debug.print("Part 2: {d}\n", .{power_sum});
}

const test_input =
    \\Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    \\Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    \\Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    \\Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    \\Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
;
