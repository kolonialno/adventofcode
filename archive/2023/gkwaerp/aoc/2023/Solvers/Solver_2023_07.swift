//
//  Solver_2023_07.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 07/12/2023.
//

import Foundation

final class Solver_2023_07: Solver {
    private var input: [String] = []

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsStringArray()
    }

    override func solveFunction1() -> CustomStringConvertible {
        let hands = input.map { Hand($0, jokerRule: false) }
        return getWinnings(hands)
    }

    override func solveFunction2() -> CustomStringConvertible {
        let hands = input.map { Hand($0, jokerRule: true) }
        return getWinnings(hands)
    }

    func getWinnings(_ hands: [Hand]) -> Int {
        let sorted = hands.sorted { h1, h2 in
            return !h1.winsAgainst(other: h2)
        }

        return sorted.enumerated().reduce(0, { $0 + ($1.offset + 1) * $1.element.bid })
    }
}

extension Solver_2023_07: TestableDay {
    func runTests() {
        let input = """
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"""
            .components(separatedBy: .newlines)

        let hands1 = input.map { Hand($0, jokerRule: false) }
        let result1 = getWinnings(hands1)
        let expected1 = 6440
        assert(result1 == expected1)

        let hands2 = input.map { Hand($0, jokerRule: true) }
        let result2 = getWinnings(hands2)
        let expected2 = 5905
        assert(result2 == expected2)
    }
}

extension Solver_2023_07 {
    struct Hand {
        let cards: [Card]
        let bid: Int
        let handType: HandType

        init(_ string: String, jokerRule: Bool) {
            let split = string.components(separatedBy: " ")
            self.cards = split[0].convertToStringArray().map { Card($0, jokerRule: jokerRule) }
            self.bid = split[1].intValue!
            self.handType = HandType(cards, jokerRule: jokerRule)
        }

        func winsAgainst(other: Hand) -> Bool {
            if handType != other.handType {
                return handType.rawValue > other.handType.rawValue
            }

            for i in 0..<cards.count {
                if cards[i].value != other.cards[i].value {
                    return cards[i].value > other.cards[i].value
                }
            }

            fatalError("Identical hands")
        }
    }
}

extension Solver_2023_07.Hand {
    struct Card {
        let label: String
        let value: Int

        init(_ string: String, jokerRule: Bool) {
            self.label = string

            let lookupString = jokerRule ? "J23456789TQKA" : "23456789TJQKA"
            let lookup = lookupString.convertToStringArray()
            self.value = lookup.firstIndex(of: string)!
        }
    }

    enum HandType: Int {
        case highCard
        case onePair
        case twoPair
        case threeOfAKind
        case fullHouse
        case fourOfAKind
        case fiveOfAKind

        init(_ cards: [Card], jokerRule: Bool) {
            let labels = cards.map { $0.label }
            let uniqueLabels = labels
                .filter { jokerRule ? $0 != "J" : true }
                .uniqueElements()

            var occurrences: [Int: Int] = [:]
            for uniqueLabel in uniqueLabels {
                let count = labels
                    .filter { $0 == uniqueLabel }
                    .count
                occurrences[count, default: 0] += 1
            }

            let numJokers = jokerRule ? labels.filter { $0 == "J" }.count : 0

            if !jokerRule || numJokers == 0 {
                if occurrences[5] == 1 {
                    self = .fiveOfAKind
                } else if occurrences[4] == 1 {
                    self = .fourOfAKind
                } else if occurrences[3] == 1 {
                    self = occurrences[2] == 1 ? .fullHouse : .threeOfAKind
                } else if occurrences[2, default: 0] > 0 {
                    self = occurrences[2] == 2 ? .twoPair : .onePair
                } else {
                    self = .highCard
                }
            } else {
                if numJokers == 5 {
                    self = .fiveOfAKind
                } else if occurrences[5 - numJokers, default: 0] >= 1 {
                    self = .fiveOfAKind
                } else if occurrences[4 - numJokers, default: 0] >= 1 {
                    self = .fourOfAKind
                } else if occurrences[2] == 2 && numJokers == 1 {
                    self = .fullHouse
                } else if occurrences[3 - numJokers, default: 0] >= 1 {
                    self = .threeOfAKind
                } else {
                    self = numJokers == 1 ? .onePair : .highCard
                }
            }
        }
    }
}
