//
//  Solver_2023_02.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 02/12/2023.
//

import Foundation

final class Solver_2023_02: Solver {
    final class GameInfo {
        struct GameSet {
            let red: Int
            let green: Int
            let blue: Int
        }

        let id: Int
        let gameSets: [GameSet]

        init(_ string: String) {
            let split = string
                .replacingOccurrences(of: "Game ", with: "")
                .components(separatedBy: ": ")

            let id = split[0].intValue!

            let sets = split[1].components(separatedBy: "; ")
            let gameSets = sets.map { setString in
                let cubeSplit = setString.components(separatedBy: ", ")
                var r = 0
                var g = 0
                var b = 0

                cubeSplit.forEach { cubeString in
                    if cubeString.contains("red") {
                        b = cubeString.replacingOccurrences(of: " red", with: "").intValue!
                    } else if cubeString.contains("green") {
                        r = cubeString.replacingOccurrences(of: " green", with: "").intValue!
                    } else {
                        g = cubeString.replacingOccurrences(of: " blue", with: "").intValue!
                    }
                }

                return GameInfo.GameSet(red: r, green: g, blue: b)
            }

            self.id = id
            self.gameSets = gameSets
        }

        func isPossible(r: Int, g: Int, b: Int) -> Bool {
            gameSets.allSatisfy { $0.red <= r && $0.green <= g && $0.blue <= b }
        }

        var power: Int {
            let r = gameSets.max(by: { $0.red < $1.red })!.red
            let g = gameSets.max(by: { $0.green < $1.green })!.green
            let b = gameSets.max(by: { $0.blue < $1.blue })!.blue

            return r * g * b
        }
    }

    private var games: [GameInfo] = []

    override func didLoadFunction() {
        games = defaultInputFileString.loadAsStringArray()
            .map { GameInfo($0) }
    }

    func getPossibleIdSum(_ games: [GameInfo]) -> Int {
        games.filter { $0.isPossible(r: 12, g: 13, b: 14) }.map { $0.id }.reduce(0, +)
    }

    func getPower(_ games: [GameInfo]) -> Int {
        games.map { $0.power }.reduce(0, +)
    }

    override func solveFunction1() -> CustomStringConvertible {
        getPossibleIdSum(games)
    }

    override func solveFunction2() -> CustomStringConvertible {
        getPower(games)
    }
}

extension Solver_2023_02: TestableDay {
    func runTests() {
        let input = """
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"""
            .components(separatedBy: .newlines)
            .map { GameInfo($0) }


        let result1 = getPossibleIdSum(input)
        let expected1 = 8
        assert(result1 == expected1)

        let result2 = getPower(input)
        let expected2 = 2286
        assert(result2 == expected2)
    }
}
