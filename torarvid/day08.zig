const std = @import("std");
const util = @import("util.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var lines = try util.file_as_strings("inputs/day08.txt", gpa.allocator());

    const commands = lines[0];

    var map = std.StringHashMap(*Node).init(gpa.allocator());

    for (lines[2..]) |line| {
        var it = std.mem.tokenizeAny(u8, line, " =(),");
        const name = it.next().?;
        const left = it.next().?;
        const right = it.next().?;
        var node = try gpa.allocator().create(Node);
        node.* = .{ .name = name, .left = left, .right = right };
        try map.put(name, node);
    }

    var node_it = NodeIterator{ .map = &map, .commands = commands, .current = map.get("AAA") };
    var count: usize = 0;
    while (try node_it.next()) |node| {
        count += 1;
        if (std.mem.eql(u8, node.name, "ZZZ")) {
            break;
        }
    }

    std.debug.print("Part 1: {}\n", .{count});

    var answer: usize = 1;
    var map_it = map.iterator();
    while (map_it.next()) |entry| {
        if (!std.ascii.endsWithIgnoreCase(entry.key_ptr.*, "A")) {
            continue;
        }
        const curr = entry.value_ptr.*;
        node_it = NodeIterator{ .map = &map, .commands = commands, .current = curr };
        count = 0;
        while (try node_it.next()) |node| {
            count += 1;
            if (std.ascii.endsWithIgnoreCase(node.name, "Z")) {
                break;
            }
        }
        answer = least_common_multiple(usize, answer, count);
    }

    std.debug.print("Part 2: {}\n", .{answer});
}

const NodeIterator = struct {
    map: *std.StringHashMap(*Node),
    commands: []const u8,
    current: ?*Node,

    var cmd_index: usize = 0;

    pub fn next(self: *NodeIterator) !?*Node {
        var node: *Node = self.current orelse return null;
        const cmd = self.commands[cmd_index];
        cmd_index += 1;
        if (cmd_index == self.commands.len) {
            cmd_index = 0;
        }
        switch (cmd) {
            'L' => self.current = self.map.get(node.left).?,
            'R' => self.current = self.map.get(node.right).?,
            else => return error.OutOfRange,
        }

        return self.current;
    }
};

fn greatest_common_divisor(comptime T: type, a: T, b: T) T {
    if (b == 0) {
        return a;
    }
    return greatest_common_divisor(T, b, a % b);
}

fn least_common_multiple(comptime T: type, a: T, b: T) T {
    return (a * b) / greatest_common_divisor(T, a, b);
}

const Node = struct {
    name: []const u8,
    right: []const u8,
    left: []const u8,
};
