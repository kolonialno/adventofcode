const std = @import("std");
const util = @import("util.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var lines = try util.file_as_strings("inputs/day05.txt", gpa.allocator());

    const seeds_raw = lines[0][7..];
    var seeds = std.ArrayList(usize).init(gpa.allocator());
    var it = std.mem.tokenizeAny(u8, seeds_raw, " ");
    while (it.next()) |seed_raw| {
        const seed = try std.fmt.parseInt(usize, seed_raw, 10);
        try seeds.append(seed);
    }

    var line_offset: usize = 3;
    const seed_to_soil_maps = try parse_maps(gpa.allocator(), lines[line_offset..]);
    line_offset += seed_to_soil_maps.len() + 2;
    const soil_to_fert_maps = try parse_maps(gpa.allocator(), lines[line_offset..]);
    line_offset += soil_to_fert_maps.len() + 2;
    const fert_to_water_maps = try parse_maps(gpa.allocator(), lines[line_offset..]);
    line_offset += fert_to_water_maps.len() + 2;
    const water_to_light_maps = try parse_maps(gpa.allocator(), lines[line_offset..]);
    line_offset += water_to_light_maps.len() + 2;
    const light_to_temp_maps = try parse_maps(gpa.allocator(), lines[line_offset..]);
    line_offset += light_to_temp_maps.len() + 2;
    const temp_to_humid_maps = try parse_maps(gpa.allocator(), lines[line_offset..]);
    line_offset += temp_to_humid_maps.len() + 2;
    const humid_to_loc_maps = try parse_maps(gpa.allocator(), lines[line_offset..]);

    var min_loc: ?usize = null;
    for (seeds.items) |seed| {
        const soil = seed_to_soil_maps.map(seed);
        const fert = soil_to_fert_maps.map(soil);
        const water = fert_to_water_maps.map(fert);
        const light = water_to_light_maps.map(water);
        const temp = light_to_temp_maps.map(light);
        const humid = temp_to_humid_maps.map(temp);
        const loc = humid_to_loc_maps.map(humid);
        if (min_loc) |min| {
            if (loc < min) {
                min_loc = loc;
            }
        } else {
            min_loc = loc;
        }
    }

    std.debug.print("Part 1: {d}\n", .{min_loc.?});

    min_loc = null;
    var i: usize = 0;
    while (i < seeds.items.len) : (i += 2) {
        const start = seeds.items[i];
        const len = seeds.items[i + 1];
        for (start..start + len) |seed| {
            const soil = seed_to_soil_maps.map(seed);
            const fert = soil_to_fert_maps.map(soil);
            const water = fert_to_water_maps.map(fert);
            const light = water_to_light_maps.map(water);
            const temp = light_to_temp_maps.map(light);
            const humid = temp_to_humid_maps.map(temp);
            const loc = humid_to_loc_maps.map(humid);
            if (min_loc) |min| {
                if (loc < min) {
                    min_loc = loc;
                }
            } else {
                min_loc = loc;
            }
        }
    }

    std.debug.print("Part 2: {d}\n", .{min_loc.?});
}

fn parse_maps(allocator: std.mem.Allocator, lines: []const []const u8) !Map {
    var maps = std.ArrayList(MapEntry).init(allocator);

    for (lines) |line| {
        if (std.mem.eql(u8, line, "")) {
            break;
        }
        var i = std.mem.tokenizeAny(u8, line, " ");
        const dest = try std.fmt.parseInt(usize, i.next().?, 10);
        const src = try std.fmt.parseInt(usize, i.next().?, 10);
        const len = try std.fmt.parseInt(usize, i.next().?, 10);
        const map = MapEntry{ .dest = dest, .src = src, .len = len };
        try maps.append(map);
    }

    return Map{ .entries = maps };
}

const MapEntry = struct {
    dest: usize,
    src: usize,
    len: usize,
};

const Map = struct {
    entries: std.ArrayList(MapEntry),

    const Self = @This();

    fn map(self: *const Self, src: usize) usize {
        for (self.entries.items) |entry| {
            if (src >= entry.src and src < entry.src + entry.len) {
                const offset = src - entry.src;
                return entry.dest + offset;
            }
        }
        return src;
    }

    fn len(self: *const Self) usize {
        return self.entries.items.len;
    }
};
