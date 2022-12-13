//
//  Solver_2022_13.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 13/12/2022.
//

import Foundation

fileprivate protocol Packet {
    var rawString: String? { get } // Used to find divider packets in part 2
}

class Solver_2022_13: Solver {
    fileprivate struct PacketList: Packet {
        let packets: [Packet]
        var rawString: String?

        init(packets: [Packet], rawString: String? = nil) {
            self.packets = packets
            self.rawString = rawString
        }
    }

    fileprivate struct PacketInt: Packet {
        let value: Int
        let rawString: String? = nil
    }

    fileprivate class Parser {
        static func parse(string: String, initialCall: Bool = true) -> PacketList {
            guard !string.isEmpty else {
                return PacketList(packets: [])
            }

            // No more nested lists --> has to be list of ints
            if !string.contains(where: { $0 == "[" }) {
                let packetInts = string
                    .components(separatedBy: ",")
                    .filter { !$0.isEmpty }
                    .map { Int($0)! }
                    .map { PacketInt(value: $0) }

                return PacketList(packets: packetInts)
            }

            var arrayed = string.convertToStringArray()

            // Here be more nested lists
            var packets: [Packet] = []
            var tmpString = ""

            var bracketStack: [Int] = []
            var index = 0
            while index < arrayed.count {
                let character = arrayed[index]
                if character == "[" {
                    bracketStack.append(index)
                } else if character == "]" {
                    let matchingBracketIndex = bracketStack.popLast()!
                    if bracketStack.isEmpty {
                        let substring = arrayed[(matchingBracketIndex + 1)..<index].joined()
                        var parsedList = parse(string: substring, initialCall: false)

                        if initialCall {
                            parsedList.rawString = string
                            return parsedList
                        }

                        packets.append(parsedList)
                        arrayed.removeSubrange(matchingBracketIndex...index)
                        index = matchingBracketIndex
                        continue
                    }
                } else if bracketStack.isEmpty {
                    if character == "," {
                        if let intValue = Int(tmpString) {
                            packets.append(PacketInt(value: intValue))
                        }
                        tmpString = ""
                    } else {
                        tmpString += character
                    }
                }

                index += 1
            }

            if let intValue = Int(tmpString) {
                packets.append(PacketInt(value: intValue))
            }

            return PacketList(packets: packets)
        }
    }

    fileprivate class Comparer {
        enum Comparison {
            case leftSmaller
            case equal
            case rightSmaller
        }

        static func compare(lhs: Packet, rhs: Packet) -> Comparison {
            switch (lhs, rhs) {
            case (let lhs as PacketList, let rhs as PacketList):
                let maxCount = max(lhs.packets.count, rhs.packets.count)
                for i in 0..<maxCount {
                    // If something runs out first, it is smaller
                    if lhs.packets.count <= i {
                        return .leftSmaller
                    } else if rhs.packets.count <= i {
                        return .rightSmaller
                    }

                    let comparison = compare(lhs: lhs.packets[i], rhs: rhs.packets[i])
                    switch comparison {
                    case .equal:
                        continue
                    case .leftSmaller, .rightSmaller:
                        return comparison
                    }
                }
                return .equal
            case (let lhs as PacketInt, let rhs as PacketInt):
                if lhs.value == rhs.value {
                    return .equal
                }

                return lhs.value < rhs.value ? .leftSmaller : .rightSmaller
            case (let lhs as PacketList, let rhs as PacketInt):
                return compare(lhs: lhs, rhs: PacketList(packets: [rhs]))

            case (let lhs as PacketInt, let rhs as PacketList):
                return compare(lhs: PacketList(packets: [lhs]), rhs: rhs)
            default:
                fatalError()
            }
        }
    }

    class PacketManager {
        private var packetLists: [PacketList]

        private let dividerPacketStrings: [String] = ["[[2]]", "[[6]]"]
        private lazy var dividerPackets: [PacketList] = {
            return dividerPacketStrings.map { Parser.parse(string: $0) }
        }()

        init(string: String) {
            self.packetLists = string
                .components(separatedBy: .newlines)
                .filter { !$0.isEmpty }
                .map { Parser.parse(string: $0) }
        }

        func getSumOfValidPairIndices() -> Int {
            var sum = 0
            for i in stride(from: 0, to: packetLists.count, by: 2) {
                if Comparer.compare(lhs: packetLists[i], rhs: packetLists[i + 1]) == .leftSmaller {
                    sum += (i / 2) + 1
                }
            }
            return sum
        }

        func sortAndGetDecoderSignal() -> Int {
            packetLists.append(contentsOf: dividerPackets)
            packetLists = packetLists.sorted(by: { lhs, rhs in
                Comparer.compare(lhs: lhs, rhs: rhs) == .leftSmaller
            })

            var product = 1
            for i in 0..<packetLists.count {
                if dividerPacketStrings.contains(where: { $0 == packetLists[i].rawString }) {
                    product *= (i + 1)
                }
            }
            return product
        }
    }

    private var input: String = ""

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsTextString()
    }

    override func solveFunction1() -> String {
        let packetManager = PacketManager(string: input)
        let result = packetManager.getSumOfValidPairIndices()
        return "\(result)"
    }

    override func solveFunction2() -> String {
        let packetManager = PacketManager(string: input)
        let decoderSignal = packetManager.sortAndGetDecoderSignal()
        return "\(decoderSignal)"
    }
}

extension Solver_2022_13: TestableDay {
    func runTests() {
        let testInput = defaultTestInputString(suffix: "a").loadAsTextString()
        let packetManager = PacketManager(string: testInput)
        let sum = packetManager.getSumOfValidPairIndices()
        assert(sum == 13)

        let decoderSignal = packetManager.sortAndGetDecoderSignal()
        assert(decoderSignal == 140)
    }
}
