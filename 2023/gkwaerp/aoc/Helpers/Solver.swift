//
//  Solver.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2022.
//

import Foundation
import SwiftUI

protocol TestableDay {
    func runTests()
}

class Solver: ObservableObject {
    private let year: Int
    private let day: Int

    @Published private(set) final var isReady: Bool = false
    @Published private(set) final var isSolvingPart1: Bool = false
    @Published private(set) final var isSolvingPart2: Bool = false
    @Published private(set) final var resultPart1: String?
    @Published private(set) final var resultPart2: String?

    @Published private(set) final var progressPart1: String?
    @Published private(set) final var progressPart2: String?
    @Published private(set) final var progressFont1: Font?
    @Published private(set) final var progressFont2: Font?

    required init(year: Int, day: Int) {
        self.year = year
        self.day = day
    }

    /// Input loading & other prep work
    func didLoadFunction() {
    }

    /// Solve part 1
    func solveFunction1() -> CustomStringConvertible {
        "Solution not implemented"
    }

    /// Solve part 2
    func solveFunction2() -> CustomStringConvertible {
        "Solution not implemented"
    }

    /// Set to `true` if solution for part 2 cannot be started before part 1 is solved
    var part2RequiresPart1Solved: Bool {
        false
    }

    var solveType1: SolverView.SolveType {
        .text
    }

    var solveType2: SolverView.SolveType {
        .text
    }
}

extension Solver {
    final var navigationTitle: String {
        day.toDayString()
    }

    final var progressState1: ProgressState? {
        guard let progressPart1 else {
            return nil
        }

        return ProgressState(text: progressPart1, font: progressFont1)
    }

    final var progressState2: ProgressState? {
        guard let progressPart2 else {
            return nil
        }

        return ProgressState(text: progressPart2, font: progressFont1)
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
        DispatchQueue.global(qos: .userInitiated).async {
            let result = self.solveFunction1()
            DispatchQueue.main.async {
                self.resultPart1 = String(describing: result)
                self.isSolvingPart1 = false

                print("Part 1:\n\(result)")

                let elapsedTime = DateHelper.getElapsedTimeString(from: startTime)
                print("\(elapsedTime)\n")
            }
        }
    }

    final func solvePart2() {
        let startTime = Date()

        isSolvingPart2 = true
        DispatchQueue.global(qos: .userInitiated).async {
            let result = self.solveFunction2()
            DispatchQueue.main.async {
                self.resultPart2 = String(describing: result)
                self.isSolvingPart2 = false

                print("Part 2:\n\(result)")

                let elapsedTime = DateHelper.getElapsedTimeString(from: startTime)
                print("\(elapsedTime)\n")
            }
        }
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

    final func defaultTestInputString(suffix: String) -> String {
        String(format: "Test%@_%@", defaultInputFileString, suffix)
    }

    final func visualizeCurrentPart(text: String?, font: Font? = nil) {
        guard isSolvingPart1 || isSolvingPart2 else {
            return
        }
        DispatchQueue.main.async {
            if self.isSolvingPart1 {
                self.progressPart1 = text
                self.progressFont1 = font
            } else {
                self.progressPart2 = text
                self.progressFont2 = font
            }
        }
    }
    final var solveState1: SolveState {
        if let result = resultPart1 {
            return .solved(result: result)
        }

        if !isReady || isSolvingPart2 {
            return .waiting
        }

        if isSolvingPart1 {
            return .solving
        }

        return .ready
    }

    final var solveState2: SolveState {
        if let result = resultPart2 {
            return .solved(result: result)
        }

        if !isReady || isSolvingPart1 || isPart2BlockedByPart1 {
            return .waiting
        }

        if isSolvingPart2 {
            return .solving
        }

        return .ready
    }
}

extension Solver {
    static func getType(year: Int, day: Int) -> Solver.Type? {
        guard let appName = Bundle.main.infoDictionary?["CFBundleName"] as? String else {
            fatalError("No bundle name found!")
        }

        let sanitizedBundleName = appName.replacingOccurrences(of: " ", with: "_")
        let solverClassName = String.yearAndDayString(year: year, day: day, prefix: "Solver")
        let finalSolverName = "\(sanitizedBundleName).\(solverClassName)"

        return NSClassFromString(finalSolverName) as? Solver.Type
    }
}
