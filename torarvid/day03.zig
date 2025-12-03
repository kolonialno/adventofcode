const std = @import("std");
const util = @import("util.zig");

const Item = struct {
    index: usize,
    digit: u8,
};

pub fn highest_digit(bank: []const u8, offset: usize, count: usize) Item {
    for (0..9) |i| {
        const needle: []const u8 = &[1]u8{@intCast('9' - i)};
        const haystack = bank[offset .. bank.len - count + 1];
        if (std.mem.indexOf(u8, haystack, needle)) |index| {
            return .{ .index = index, .digit = @intCast(9 - i) };
        }
    }
    unreachable;
}

pub fn get_jolts(banks: std.ArrayList([]const u8), num_batteries: u8) u64 {
    var total: u64 = 0;
    for (banks.items) |bank| {
        var offset: usize = 0;
        var bank_total: u64 = 0;
        for (0..num_batteries) |i| {
            const item = highest_digit(bank, offset, num_batteries - i);
            offset += item.index + 1;
            bank_total = bank_total * 10 + item.digit;
        }
        total += bank_total;
    }
    return total;
}

pub fn run(input: []const []const u8, allocator: std.mem.Allocator) !void {
    var buf: [1024][]const u8 = undefined;
    var banks = std.ArrayList([]const u8).initBuffer(&buf);
    for (input) |batt| {
        try banks.appendBounded(batt);
    }

    const part1 = get_jolts(banks, 2);
    std.debug.print("Part 1: {d}\n", .{part1});
    const part2 = get_jolts(banks, 12);
    std.debug.print("Part 2: {d}\n", .{part2});

    for (input) |line| {
        allocator.free(line);
    }
    allocator.free(input);
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const input = try util.file_as_strings("inputs/day03.txt", allocator);
    try run(input, allocator);
}

test "day02" {
    const raw =
        \\987654321111111
        \\811111111111119
        \\234234234234278
        \\818181911112111
    ;
    var reader = std.Io.Reader.fixed(raw);

    const allocator = std.testing.allocator;
    const input = try util.read_as_strings(&reader, allocator);
    try run(input, allocator);
}
