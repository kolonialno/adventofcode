//
//  TravellingSalesmanHelper.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2022.
//

import Foundation

class TravellingSalesmanHelper {
    struct Result {
        let shortestDistance: Int?
        let shortestPath: [String]?
        let longestDistance: Int?
        let longestPath: [String]?
    }

    /// Key = "Location1-Location2"
    /// Value = Distance
    /// Example: "Paris-London" : 344
    typealias DistanceDictionary = [String : Int]
    static func findShortestDistance(locationPermutations: [[String]], distanceDictionary: DistanceDictionary) -> Result {
        var shortestDistance: Int? = nil
        var shortestPath: [String]? = nil

        for permutation in locationPermutations {
            var currDistance = 0
            for i in 0..<permutation.count - 1 {
                let key = "\(permutation[i])-\(permutation[i + 1])"
                let distance = distanceDictionary[key]!
                currDistance += distance

                if currDistance > (shortestDistance ?? Int.max) {
                    break
                }
            }

            if currDistance < (shortestDistance ?? Int.max) {
                shortestDistance = currDistance
                shortestPath = permutation
            }
        }

        return Result(shortestDistance: shortestDistance,
                      shortestPath: shortestPath,
                      longestDistance: nil,
                      longestPath: nil)
    }

    static func findLongestDistance(locationPermutations: [[String]], distanceDictionary: DistanceDictionary) -> Result {
        var longestDistance: Int? = nil
        var longestPath: [String]? = nil

        for permutation in locationPermutations {
            var currDistance = 0
            for i in 0..<permutation.count - 1 {
                let key = "\(permutation[i])-\(permutation[i + 1])"
                let distance = distanceDictionary[key]!
                currDistance += distance
            }

            if currDistance > (longestDistance ?? 0) {
                longestDistance = currDistance
                longestPath = permutation
            }
        }

        return Result(shortestDistance: nil,
                      shortestPath: nil,
                      longestDistance: longestDistance,
                      longestPath: longestPath)
    }

    static func findDistances(locationPermutations: [[String]], distanceDictionary: DistanceDictionary) -> Result {
        var shortestDistance: Int? = nil
        var shortestPath: [String]? = nil
        var longestDistance: Int? = nil
        var longestPath: [String]? = nil

        for permutation in locationPermutations {
            var currDistance = 0
            for i in 0..<permutation.count - 1 {
                let key = "\(permutation[i])-\(permutation[i + 1])"
                let distance = distanceDictionary[key]!
                currDistance += distance
            }

            if currDistance < (shortestDistance ?? Int.max) {
                shortestDistance = currDistance
                shortestPath = permutation
            }

            if currDistance > (longestDistance ?? 0) {
                longestDistance = currDistance
                longestPath = permutation
            }
        }

        return Result(shortestDistance: shortestDistance,
                      shortestPath: shortestPath,
                      longestDistance: longestDistance,
                      longestPath: longestPath)
    }
}
