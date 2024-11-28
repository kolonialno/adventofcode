//
//  Solver_2017_16.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 27/11/2023.
//

import Foundation

final class Solver_2017_16: Solver {
    enum DanceMove {
        case spin(Int)
        case exchange(Int, Int)
        case partner(String, String)

        init(_ string: String) {
            if string.hasPrefix("s") {
                let spinValue = string.replacingOccurrences(of: "s", with: "").intValue!
                self = .spin(spinValue)
            } else if string.hasPrefix("x") {
                let split = string.replacingOccurrences(of: "x", with: "").components(separatedBy: "/")
                self = .exchange(split[0].intValue!, split[1].intValue!)
            } else if string.hasPrefix("p") {
                let newString = string.convertToStringArray()[1...].joined()
                let split = newString.components(separatedBy: "/")
                self = .partner(split[0], split[1])
            } else {
                fatalError("Invalid dance move")
            }
        }
    }

    final class Dance {
        var programs: [String]
        let numPrograms: Int

        init(programNames: String) {
            programs = programNames.map { "\($0)" }
            numPrograms = programs.count
        }

        func perform(_ danceMoves: [DanceMove], numTimes: Int = 1) {
            var history: [String: Int] = [:]
            var cycleStarted = false
            var cycleLength = 0
            var numPerformed = 0
            history[getProgramOrder(), default: 0] += 1

            for _ in 0..<numTimes {
                danceMoves.forEach { perform($0) }
                numPerformed += 1
                history[getProgramOrder(), default: 0] += 1
                if cycleStarted {
                    cycleLength += 1
                    if history[getProgramOrder()] == 3 {
                        break
                    }
                } else {
                    if history[getProgramOrder()] == 2 {
                        cycleStarted = true
                    }
                }
            }

            let remaining = numTimes - numPerformed
            if remaining > 0 {
                let actualRemaining = remaining % cycleLength
                for _ in 0..<actualRemaining {
                    danceMoves.forEach { perform($0) }
                }
            }
        }

        private func perform(_ danceMove: DanceMove) {
            switch danceMove {
            case .spin(let length):
                let newStart = programs.suffix(length)
                let newEnd = programs.prefix(numPrograms - length)
                programs = Array(newStart) + Array(newEnd)
            case .exchange(let index1, let index2):
                programs.swapAt(index1, index2)
            case .partner(let name1, let name2):
                let index1 = programs.firstIndex(where: { $0 == name1 })!
                let index2 = programs.firstIndex(where: { $0 == name2 })!
                programs.swapAt(index1, index2)
            }
        }

        func getProgramOrder() -> String {
            programs.joined()
        }
    }

    private var danceMoves: [DanceMove] = []

    override func didLoadFunction() {
        danceMoves = defaultInputFileString.loadAsTextString()
            .components(separatedBy: ",")
            .map { DanceMove($0) }
    }

    override func solveFunction1() -> CustomStringConvertible {
        let dance = Dance(programNames: "abcdefghijklmnop")
        dance.perform(danceMoves)
        return dance.getProgramOrder()
    }

    override func solveFunction2() -> CustomStringConvertible {
        let dance = Dance(programNames: "abcdefghijklmnop")
        dance.perform(danceMoves, numTimes: 1_000_000_000)
        return dance.getProgramOrder()
    }
}

extension Solver_2017_16: TestableDay {
    func runTests() {
        let input = "s1,x3/4,pe/b"
            .components(separatedBy: ",")
        let danceMoves = input.map { DanceMove($0) }
        let dance = Dance(programNames: "abcde")

        dance.perform(danceMoves)
        let result = dance.getProgramOrder()
        let expected = "baedc"
        assert(result == expected)

        var results = [result]
        for _ in 1..<16 {
            dance.perform(danceMoves)
            results.append(dance.getProgramOrder())
        }

        var multiDanceResults: [String] = []
        for i in 1...16 {
            let newDance = Dance(programNames: "abcde")
            newDance.perform(danceMoves, numTimes: i)
            multiDanceResults.append(newDance.getProgramOrder())
        }

        assert(results == multiDanceResults)
    }
}
