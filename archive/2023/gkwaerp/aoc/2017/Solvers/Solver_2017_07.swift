//
//  Solver_2017_07.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 14/11/2023.
//

import Foundation

final class Solver_2017_07: Solver {
    private struct Tower {
        let name: String
        let weight: Int
        let children: [String]

        init(_ string: String) {
            let mainSplit = string.components(separatedBy: " -> ")
            let nameWeight = mainSplit[0].components(separatedBy: " ")
            self.name = nameWeight[0]
            self.weight = nameWeight[1]
                .replacingOccurrences(of: "(", with: "")
                .replacingOccurrences(of: ")", with: "")
                .intValue!

            if mainSplit.count > 1 {
                children = mainSplit[1]
                    .replacingOccurrences(of: ",", with: "")
                    .components(separatedBy: " ")
            } else {
                children = []
            }
        }
    }

    private struct BalanceInfo {
        let targetWeight: Int
        let nextNode: TreeNode<Tower>
    }

    private var towers: [Tower] = []

    override func didLoadFunction() {
        towers = defaultInputFileString.loadAsStringArray().map { Tower($0) }
    }

    override func solveFunction1() -> CustomStringConvertible {
        let result = getTowerRoot(towers)
        return result.value.name
    }

    override func solveFunction2() -> CustomStringConvertible {
        let result = getRebalancedWeight(towers)!
        return result
    }

    private func getTowerRoot(_ towers: [Tower]) -> TreeNode<Tower> {
        // Name -> TowerNode
        var nodes: [String: TreeNode<Tower>] = [:]
        towers.forEach { nodes[$0.name] = TreeNode<Tower>(value: $0, children: [])  }
        towers.forEach { tower in
            let towerNode = nodes[tower.name]!
            for child in tower.children {
                let childNode = nodes[child]!
                towerNode.children.append(childNode)
                childNode.parent = towerNode
            }
        }

        var root = nodes[towers.first!.name]!
        while let parent = root.parent {
            root = parent
        }

        return root
    }

    private func getRebalancedWeight(_ towers: [Tower]) -> Int? {
        var node = getTowerRoot(towers)
        var targetWeight = 0

        while !isBalanced(node) {
            let balanceInfo = getBalanceInfo(node)
            node = balanceInfo.nextNode
            targetWeight = balanceInfo.targetWeight
        }

        let weightDelta = targetWeight - getTotalWeight(for: node)
        return node.value.weight + weightDelta
    }

    private func getBalanceInfo(_ node: TreeNode<Tower>) -> BalanceInfo {
        let groups = Dictionary(grouping: node.children, by: { getTotalWeight(for: $0) })
        let targetWeight = groups.values
            .first { $0.count > 1 }!
            .first.map { getTotalWeight(for: $0) }!

        let nextNode = groups.values
            .first { $0.count == 1 }!
            .first!

        return .init(targetWeight: targetWeight, nextNode: nextNode)
    }

    private func isBalanced(_ node: TreeNode<Tower>) -> Bool {
        guard !node.children.isEmpty else { return true }
        let childWeights = node.children.map { getTotalWeight(for: $0) }
        let childWeightSet = Set(childWeights)
        return childWeightSet.count == 1
    }

    private func getTotalWeight(for node: TreeNode<Tower>) -> Int {
        let ownWeight = node.value.weight
        let childWeight = node.children.map { getTotalWeight(for: $0) }
            .reduce(0, +)
        return ownWeight + childWeight
    }
}

extension Solver_2017_07: TestableDay {
    func runTests() {
        let input = """
pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)
"""
            .components(separatedBy: .newlines)

        let towers = input.map { Tower($0) }
        let towerRoot = getTowerRoot(towers)
        let expected = "tknk"
        assert(towerRoot.value.name == expected)

        let rebalancedWeight = getRebalancedWeight(towers)!
        let expectedWeight = 60
        assert(rebalancedWeight == expectedWeight)
    }
}
