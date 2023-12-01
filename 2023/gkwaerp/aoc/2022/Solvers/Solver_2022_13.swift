//
//  Solver_2022_13.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 13/12/2022.
//

import Foundation

class Solver_2022_13: Solver {
    indirect enum Packet: Decodable, Comparable {
        case list([Packet])
        case value(Int)

        init(from decoder: Decoder) throws {
            do {
                let container = try decoder.singleValueContainer()
                self = .value(try container.decode(Int.self))
            } catch {
                self = .list(try [Packet](from: decoder))
            }
        }

        static func < (lhs: Packet, rhs: Packet) -> Bool {
            switch (lhs, rhs) {
            case (.list(let lhsList), .list(let rhsList)):
                for (lhsValue, rhsValue) in zip(lhsList, rhsList) {
                    if lhsValue < rhsValue { return true }
                    if lhsValue > rhsValue { return false }
                }
                return lhsList.count < rhsList.count
            case (.value(let lhsValue), .value(let rhsValue)):
                return lhsValue < rhsValue
            case (.value, .list):
                return .list([lhs]) < rhs
            case (.list, .value):
                return lhs < .list([rhs])
            }
        }
    }

    class PacketManager {
        private static let jsonDecoder = JSONDecoder()

        private var packets: [Packet]
        private let dividerPacketStrings: [String] = ["[[2]]", "[[6]]"]
        private lazy var dividerPackets: [Packet] = {
            return dividerPacketStrings.map { try! Self.jsonDecoder.decode(Packet.self, from: $0.data(using: .utf8)!) }
        }()

        init(string: String) {
            self.packets = string
                .components(separatedBy: .newlines)
                .filter { !$0.isEmpty }
                .map { try! Self.jsonDecoder.decode(Packet.self, from: $0.data(using: .utf8)!) }
        }

        func getSumOfValidPairIndices() -> Int {
            var sum = 0
            for i in stride(from: 0, to: packets.count, by: 2) {
                if packets[i] < packets[i + 1] {
                    sum += (i / 2) + 1
                }
            }
            return sum
        }

        func sortAndGetDecoderSignal() -> Int {
            packets.append(contentsOf: dividerPackets)
            packets.sort()

            var product = 1
            for i in 0..<packets.count {
                if dividerPackets.contains(where: { $0 == packets[i] }) {
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

    override func solveFunction1() -> CustomStringConvertible {
        let packetManager = PacketManager(string: input)
        let result = packetManager.getSumOfValidPairIndices()
        return "\(result)"
    }

    override func solveFunction2() -> CustomStringConvertible {
        let packetManager = PacketManager(string: input)
        let decoderSignal = packetManager.sortAndGetDecoderSignal()
        return "\(decoderSignal)"
    }
}
