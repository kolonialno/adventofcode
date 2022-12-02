//
//  Solver.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2022.
//

import Foundation

protocol TestableDay {
    func runTests()
}

class Solver: ObservableObject {
    private let year: Int
    private let day: Int

    @Published private(set) var isReady: Bool = false
    @Published private(set) var isSolvingPart1: Bool = false
    @Published private(set) var isSolvingPart2: Bool = false
    @Published private(set) var resultPart1: String?
    @Published private(set) var resultPart2: String?

    required init(year: Int, day: Int) {
        self.year = year
        self.day = day
    }

    /// Input loading & other prep work
    func didLoadFunction() {
    }

    /// Solve part 1
    func solveFunction1() -> String {
        "Solution not implemented"
    }

    /// Solve part 2
    func solveFunction2() -> String {
        "Solution not implemented"
    }

    /// Set to `true` if solution for part 2 cannot be started before part 1 is solved
    var part2RequiresPart1Solved: Bool {
        false
    }
}

extension Solver {
    final var navigationTitle: String {
        day.toDayString()
    }

    final func prepareForSolve() {
        print("---------------------\nDAY \(day)")

        doInit()


        if let testableDay = self as? TestableDay {
            doTests(testableDay)
        } else {
            print("No tests today!")
        }

        print()
        isReady = true
    }

    private func doInit() {
        let startTime = Date()

        didLoadFunction()

        let elapsedTime = DateHelper.getElapsedTimeString(from: startTime)
        print("Init completed. \(elapsedTime)")
    }

    private func doTests(_ testableDay: TestableDay) {
        let startTime = Date()

        testableDay.runTests()

        let elapsedTime = DateHelper.getElapsedTimeString(from: startTime)
        print("Tests complete. \(elapsedTime)")
    }

    final func solvePart1() {
        let startTime = Date()

        isSolvingPart1 = true
        resultPart1 = solveFunction1()
        isSolvingPart1 = false

        print("Part 1:\n\(resultPart1 ?? "NO RESULT")")

        let elapsedTime = DateHelper.getElapsedTimeString(from: startTime)
        print("\(elapsedTime)\n")
    }

    final func solvePart2() {
        let startTime = Date()

        isSolvingPart2 = true
        resultPart2 = solveFunction2()
        isSolvingPart2 = false

        print("Part 2:\n\(resultPart2 ?? "NO RESULT\n")")

        let elapsedTime = DateHelper.getElapsedTimeString(from: startTime)
        print("\(elapsedTime)\n")
    }

    final var isSolving: Bool {
        isSolvingPart1 || isSolvingPart2
    }

    final var isPart2BlockedByPart1: Bool {
        part2RequiresPart1Solved && resultPart1 == nil
    }

    final var defaultInputFileString: String {
        String.yearAndDayString(year: year, day: day, prefix: "Input")
    }

    final var solveState1: SolveState {
        if !isReady || isSolvingPart2 {
            return .waiting
        }

        if isSolvingPart1 {
            return .solving
        }

        if let result = resultPart1 {
            return .solved(result: result)
        }

        return .ready
    }

    final var solveState2: SolveState {
        if !isReady || isSolvingPart1 || isPart2BlockedByPart1 {
            return .waiting
        }

        if isSolvingPart2 {
            return .solving
        }

        if let result = resultPart2 {
            return .solved(result: result)
        }

        return .ready
    }
}
