const std = @import("std");
const Allocator = std.mem.Allocator;
const util = @import("util.zig");

const Range = struct {
    start: u64,
    end: u64,

    const Self = @This();

    pub fn init(spec: []const u8) Range {
        var it = std.mem.splitAny(u8, spec, "-");
        const first = it.next() orelse unreachable;
        const second = it.next() orelse unreachable;
        return .{
            .start = std.fmt.parseInt(u64, first, 10) catch unreachable,
            .end = std.fmt.parseInt(u64, second, 10) catch unreachable,
        };
    }

    const IterItem = struct {
        str: []const u8,
        num: u64,

        pub fn sum(self: *const @This(), len: usize) ?u64 {
            const initial = self.str[0..len];
            const num_chunks: u32 = @intCast(self.str.len / len);
            for (1..num_chunks) |offset| {
                const cmp = self.str[offset * len .. offset * len + len];
                if (!std.mem.eql(u8, initial, cmp)) {
                    return null;
                }
            }
            return self.num;
        }
    };

    const Iterator = struct {
        num: u64,
        end: u64,
        buf: [100]u8 = undefined,

        pub fn next(this: *Iterator) ?IterItem {
            if (this.num > this.end) {
                return null;
            }
            const str = std.fmt.bufPrint(&this.buf, "{d}", .{this.num}) catch unreachable;
            const item = IterItem{ .str = str, .num = this.num };
            this.num += 1;
            return item;
        }
    };

    pub fn iter(self: Self) Iterator {
        return Iterator{
            .num = self.start,
            .end = self.end,
        };
    }
};

pub fn part1(ranges: std.ArrayList(Range)) !void {
    var sum: u64 = 0;
    for (ranges.items) |range| {
        var iter = range.iter();
        while (iter.next()) |item| {
            if (item.str.len % 2 != 0) {
                continue;
            }
            sum += item.sum(item.str.len / 2) orelse 0;
        }
    }
    std.debug.print("Part 1: {d}\n", .{sum});
}

pub fn part2(ranges: std.ArrayList(Range)) !void {
    var sum: u64 = 0;
    for (ranges.items) |range| {
        var iter = range.iter();
        while (iter.next()) |item| {
            for (1..1 + (item.str.len / 2)) |len| {
                if (item.str.len % len != 0) {
                    continue;
                }
                if (item.sum(len)) |partial| {
                    sum += partial;
                    break;
                }
            }
        }
    }
    std.debug.print("Part 2: {d}\n", .{sum});
}

pub fn run(input: []const []const u8, allocator: Allocator) !void {
    var iter = std.mem.splitAny(u8, input[0], ",");
    var ranges = std.ArrayList(Range){};
    while (iter.next()) |part| {
        try ranges.append(allocator, Range.init(part));
    }
    try part1(ranges);
    try part2(ranges);
    ranges.clearAndFree(allocator);
    for (input) |line| {
        allocator.free(line);
    }
    allocator.free(input);
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const input = try util.file_as_strings("inputs/day02.txt", allocator);
    try run(input, allocator);
}

test "day02" {
    const raw =
        \\11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124
    ;
    var reader = std.Io.Reader.fixed(raw);

    const allocator = std.testing.allocator;
    const input = try util.read_as_strings(&reader, allocator);
    try run(input, allocator);
}
