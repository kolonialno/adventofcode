//
//  PermutationHelper.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2022.
//

import Foundation

class PermutationHelper {
    /// Results always include all elements.
    static func allPermutations<T>(_ elements: [T]) -> [[T]] {
        if elements.count == 1 { return [elements] }
        var allResults: [[T]] = []

        for (i, element) in elements.enumerated() {
            var elements = elements
            elements.remove(at: i)
            let permutations = allPermutations(elements)
            let p = permutations.map { (a: [T]) -> [T] in
                var b = a
                b.insert(element, at: 0)
                return b
            }
            allResults += p
        }
        return allResults
    }

    /// Results include partial list of elements.
    static func allCombinations<T>(_ elements: [T]) -> [[T]] {
        guard !elements.isEmpty else {
            return [[]]
        }

        return allCombinations(Array(elements[1...]))
            .flatMap { [$0, [elements[0]] + $0] }
    }
}
