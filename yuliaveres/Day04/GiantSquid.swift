//
//  GiantSquid.swift
//  AdventOfCode2021
//
//  Created by Yuliia Veresklia on 04/12/2021.
//

import Foundation

struct GiantSquid {

    func part1(_ input: String) throws -> Int {
        return try play(buildInput(input), mode: .first)
    }

    func part2(_ input: String) throws -> Int {
        return try play(buildInput(input), mode: .last)
    }

    private func play(_ gameInput: BingoGameInput, mode: FindMode) throws -> Int {
        var result = 0

        for number in gameInput.calledNumbers {
            var stop: Bool = false

            for board in gameInput.boards {
                board.markNumber(number)

                switch mode {
                case .first where board.won != nil:
                    stop.toggle()

                    let sum = board.items.keys.reduce(0, +)
                    result = sum * number
                    break

                case .first,
                        .last :
                    break
                }
            }

            if stop {
                break
            }
        }

        if mode == .last {
            guard let lastWonBoard = gameInput.boards.sorted(by: { $0.won! > $1.won! }).first,
                  let wonNumber = lastWonBoard.wonNumber else {
                      throw GiantSquid.Error.stoppedWithoutWinner
                  }
            
            let sum = lastWonBoard.items.keys.reduce(0, +)
            result = sum * wonNumber
        }

        return result
    }

    enum FindMode {
        case first
        case last
    }

    private func buildInput(_ input: String) -> BingoGameInput {
        let lines = input.components(separatedBy: "\n")
        let calledNumbers = lines[0]

        var boards: [Board] = []
        var boardItems: [Int: (Int, Int)] = [:]

        var y = 0

        for line in lines.dropFirst().dropFirst() {
            guard !line.isEmpty else {
                y = 0
                boards.append(Board(items: boardItems))
                boardItems = [:]

                continue
            }

            line
                .components(separatedBy: .whitespaces)
                .filter { !$0.isEmpty }
                .enumerated()
                .forEach { item in
                    guard let value = Int(String(item.element)) else { return }
                    boardItems[value] = (item.offset, y)
                }

            y += 1
        }

        boards.append(Board(items: boardItems))

        return BingoGameInput(
            calledNumbers: calledNumbers.components(separatedBy: ",").compactMap { Int($0) },
            boards: boards)
    }
}

extension GiantSquid {

    struct BingoGameInput {

        let calledNumbers: [Int]

        var boards: [Board]
    }

    final class Board {

        var items: [Int: (Int, Int)]

        init(items: [Int: (Int, Int)]) {
            self.items = items
        }

        /// [0, 0, 0, 0, 0]
        /// [0, 2, 3, 0, 5]
        private var countMarkedY = Array(repeating: 0, count: 5)
        private var countMarkedX = Array(repeating: 0, count: 5)

        var won: Date?
        var wonNumber: Int?

        func markNumber(_ number: Int) {
            guard won == nil else { return }

            if let removed = items.removeValue(forKey: number) {
                countMarkedX[removed.0] += 1
                countMarkedY[removed.1] += 1

                if countMarkedX.contains(5) || countMarkedY.contains(5) {
                    won = Date()
                    wonNumber = number
                }
            }
        }
    }

    enum Error: LocalizedError {
        case stoppedWithoutWinner
    }
}
