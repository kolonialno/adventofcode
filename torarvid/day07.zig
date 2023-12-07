const std = @import("std");
const util = @import("util.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var lines = try util.file_as_strings("inputs/day07.txt", gpa.allocator());

    var hands = std.ArrayList(Hand).init(gpa.allocator());
    for (lines) |line| {
        var hand = try Hand.init(line[0..5], line[6..]);
        try hands.append(hand);
    }
    std.sort.pdq(Hand, hands.items, {}, Hand.compare);

    var sum: usize = 0;
    for (hands.items, 1..) |hand, rank| {
        sum += hand.bet * rank;
    }
    std.debug.print("Part 1: {}\n", .{sum});

    hands.clearRetainingCapacity();
    for (lines) |line| {
        var hand = try Hand.initWithJokers(line[0..5], line[6..]);
        try hands.append(hand);
    }
    std.sort.pdq(Hand, hands.items, {}, Hand.compare);

    sum = 0;
    for (hands.items, 1..) |hand, rank| {
        sum += hand.bet * rank;
    }
    std.debug.print("Part 2: {}\n", .{sum});
}

const Cards = std.ComptimeStringMap(u4, .{
    .{ "A", 14 },
    .{ "K", 13 },
    .{ "Q", 12 },
    .{ "J", 11 },
    .{ "T", 10 },
    .{ "9", 9 },
    .{ "8", 8 },
    .{ "7", 7 },
    .{ "6", 6 },
    .{ "5", 5 },
    .{ "4", 4 },
    .{ "3", 3 },
    .{ "2", 2 },
    .{ "R", 1 }, // JokeRRRRRRR
});

const HandType = enum(u3) {
    HighCard,
    Pair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
};

const Hand = struct {
    cards: [5]u4,
    bet: usize,
    typ: HandType,

    const Self = @This();

    fn init(cards_raw: *const [5]u8, bet_raw: []const u8) !Self {
        var counts: [14]u3 = [_]u3{0} ** 14;
        var cards: [5]u4 = undefined;
        const bet = try std.fmt.parseInt(usize, bet_raw, 10);
        for (0..5) |i| {
            const card = cards_raw[i .. i + 1];
            cards[i] = Cards.get(card) orelse return error.OutOfRange;
            counts[cards[i] - 1] += 1;
        }
        const jokers = counts[0];
        var have_five = false;
        var have_four = false;
        var have_three = false;
        var pairs: u2 = 0;
        for (counts[1..]) |count| {
            if (count + jokers == 5) {
                have_five = true;
            } else if (count + jokers == 4) {
                have_four = true;
            } else if (count == 3) {
                have_three = true;
            } else if (count == 2) {
                pairs += 1;
            }
        }
        if (have_five) {
            return .{ .cards = cards, .typ = .FiveOfAKind, .bet = bet };
        }
        if (have_four) {
            return .{ .cards = cards, .typ = .FourOfAKind, .bet = bet };
        }
        if (have_three) {
            if (pairs == 1 or jokers > 0) {
                return .{ .cards = cards, .typ = .FullHouse, .bet = bet };
            } else {
                return .{ .cards = cards, .typ = .ThreeOfAKind, .bet = bet };
            }
        }
        if (pairs == 2) {
            if (jokers > 0) {
                return .{ .cards = cards, .typ = .FullHouse, .bet = bet };
            }
            return .{ .cards = cards, .typ = .TwoPair, .bet = bet };
        } else if (pairs == 1) {
            if (jokers > 0) {
                return .{ .cards = cards, .typ = .ThreeOfAKind, .bet = bet };
            }
            return .{ .cards = cards, .typ = .Pair, .bet = bet };
        }
        if (jokers == 2) {
            return .{ .cards = cards, .typ = .ThreeOfAKind, .bet = bet };
        } else if (jokers == 1) {
            return .{ .cards = cards, .typ = .Pair, .bet = bet };
        }
        return .{ .cards = cards, .typ = .HighCard, .bet = bet };
    }

    fn initWithJokers(cards_raw: *const [5]u8, bet_raw: []const u8) !Self {
        var replaced: [5]u8 = undefined;
        _ = std.mem.replace(u8, cards_raw, "J", "R", &replaced);
        return try Self.init(&replaced, bet_raw);
    }

    fn compare(context: void, a: Hand, b: Hand) bool {
        if (a.typ != b.typ) {
            return std.sort.asc(u3)(context, @intFromEnum(a.typ), @intFromEnum(b.typ));
        }
        for (0..5) |i| {
            const card_a = a.cards[i];
            const card_b = b.cards[i];
            if (card_a != card_b) {
                return std.sort.asc(u4)(context, card_a, card_b);
            }
        }
        return true;
    }
};
