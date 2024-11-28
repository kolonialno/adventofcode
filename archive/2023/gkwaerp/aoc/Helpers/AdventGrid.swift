//
//  AdventGrid.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2022.
//

import Foundation

class AdventGrid<GridValue> {
    typealias PrintBlock = (IntPoint, GridValue) -> (String?)
    typealias GridStorage = [IntPoint: GridValue]
    private var storage: GridStorage

    var size: IntPoint

    var values: GridStorage.Values {
        storage.values
    }

    var width: Int {
        size.x
    }
    var height: Int {
        size.y
    }

    lazy var gridPoints: [IntPoint] = {
        size.gridPoints
    }()

    init(size: IntPoint, storage: GridStorage) {
        guard size.x > 0, size.y > 0 else {
            fatalError("Invalid grid, size must be non-negative in both axes.")
        }

        guard size.x * size.y == storage.count else {
            fatalError("Invalid grid, size must match element count.")
        }

        self.size = size
        self.storage = storage
    }

    convenience init(size: IntPoint, fillWith value: GridValue) {
        guard size.x > 0, size.y > 0 else {
            fatalError("Invalid grid, size must be non-negative in both axes.")
        }

        let numCells = size.x * size.y
        let values: [GridValue] = (0..<numCells).map { _ in return value }
        self.init(size: size, values: values)
    }

    /// Square grid
    convenience init(values: [[GridValue]]) {
        let numRows = values.count
        guard let firstRow = values.first else {
            fatalError("Invalid grid, must contain at least 1 row.")
        }

        let size = IntPoint(x: firstRow.count, y: numRows)
        let flattened = Array(values.joined())

        guard size.x * size.y == flattened.count else {
            fatalError("Invalid grid, must be square.")
        }

        var storage: GridStorage = [:]
        for y in 0..<values.count {
            for x in 0..<values[y].count {
                storage[IntPoint(x: x, y: y)] = values[y][x]
            }
        }

        self.init(size: size, storage: storage)
    }

    convenience init(grid: AdventGrid) {
        self.init(size: grid.size, storage: grid.storage)
    }

    convenience init(size: IntPoint, values: [GridValue]) {
        guard size.x > 0, size.y > 0 else {
            fatalError("Invalid grid, size must be non-negative in both axes.")
        }

        guard size.x * size.y == values.count else {
            fatalError("Invalid grid, size must match element count.")
        }

        var storage: GridStorage = [:]
        for i in 0..<values.count {
            let x = i % size.x
            let y = i / size.x
            storage[IntPoint(x: x, y: y)] = values[i]
        }

        self.init(size: size, storage: storage)
    }

    func updateStorage(_ newStorage: GridStorage) {
        storage = newStorage
    }

    func isWithinBounds(_ position: IntPoint) -> Bool {
        guard position.x < width, position.x >= 0 else {
            return false
        }

        guard position.y < height, position.y >= 0 else {
            return false
        }

        return true
    }

    func getValue(at position: IntPoint) -> GridValue? {
        guard isWithinBounds(position) else {
            return nil
        }

        return storage[position]
    }

    func setValue(at position: IntPoint, to value: GridValue) {
        guard isWithinBounds(position) else {
            return
        }

        storage[position] = value
    }

    func getValues(offset from: IntPoint, offsets: [IntPoint]) -> [GridValue] {
        offsets.compactMap { getValue(at: from + $0) }
    }

    func getValues(matching filter: (GridValue) -> (Bool)) -> [GridValue] {
        values.filter(filter)
    }

    func positions(matching filter: (GridValue) -> (Bool)) -> [IntPoint] where GridValue: Equatable {
        gridPoints.filter { filter(getValue(at: $0)!) }
    }

    func firstPosition(matching filter: (GridValue) -> (Bool)) -> IntPoint? where GridValue: Equatable {
        gridPoints.first { filter(getValue(at: $0)!) }
    }

    func asText(printClosure: PrintBlock) -> String {
        var finalText = "\n"
        for y in 0..<height {
            for x in 0..<width {
                let position = IntPoint(x: x, y: y)
                if let value = getValue(at: position),
                    let outputString = printClosure(position, value) {
                    finalText.append(outputString)
                }                }
            finalText.append("\n")
        }
        return finalText.trimmingCharacters(in: .whitespacesAndNewlines)
    }
}

// A*
extension AdventGrid {
    typealias WalkableBlock = (GridValue) -> (Bool)

    /// FromNode, ToNode. Return `Int.max` to indicate edge is not traversable
    typealias CostBlock = (IntPoint, IntPoint) -> Int
    func createAStarNodes(walkableBlock isWalkable: WalkableBlock,
                          allowedDirections: [Direction] = Direction.allCases,
                          costBlock: CostBlock) -> [IntPoint: AStarNode<IntPoint>] where GridValue: Hashable {
        var nodes: [IntPoint: AStarNode<IntPoint>] = [:]
        for point in gridPoints {
            guard let gridValue = getValue(at: point) else {
                continue
            }

            if isWalkable(gridValue) {
                nodes[point] = AStarNode(identifier: point)
            }
        }

        for node in nodes.values {
            for direction in allowedDirections {
                let newPosition = node.identifier + direction.movementVector
                guard let newValue = getValue(at: newPosition), isWalkable(newValue) else {
                    continue
                }

                let cost = costBlock(node.identifier, newPosition)
                guard cost < .max else {
                    continue
                }

                let newNode = nodes[newPosition]!
                node.edges.insert(AStarEdge(to: newNode, cost: cost))
            }
        }
        return nodes
    }
}


extension AdventGrid {
    static func defaultPrintClosure() -> PrintBlock where GridValue == String {
        return { _, value in
            return value.description
        }
    }

    static func defaultPrintClosure() -> PrintBlock where GridValue == Int {
        return { _, value in
            return "\(value)"
        }
    }

    static func defaultWalkableBlock() -> WalkableBlock where GridValue == String {
        return { value in
            switch value {
            case "#":
                return false
            default:
                return true
            }
        }
    }

    /// Each element in the array corresponds to 1 row, each element in the row corresponds to 1 cell
    convenience init(stringArray: [String]) where GridValue == String {
        var values: [GridValue] = []

        for line in stringArray {
            for char in line {
                values.append(String(char))
            }
        }

        let size = IntPoint(x: stringArray.first!.count, y: stringArray.count)
        self.init(size: size, values: values)
    }

    /// Each element in the array corresponds to 1 row, each element in the row corresponds to 1 cell
    convenience init(stringArray: [String]) where GridValue == Int {
        var values: [GridValue] = []

        for line in stringArray {
            for char in line {
                values.append(Int(String(char))!)
            }
        }

        let size = IntPoint(x: stringArray.first!.count, y: stringArray.count)
        self.init(size: size, values: values)
    }
}


typealias AdventColorGrid = AdventGrid<AdventColor>
typealias IntGrid = AdventGrid<Int>
typealias StringGrid = AdventGrid<String>
