//
//  Solver_2022_15.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 15/12/2022.
//

import Foundation

class Solver_2022_15: Solver {
    class Mapper {
        struct Sensor: Hashable {
            let position: IntPoint
            let beaconPos: IntPoint
            let distance: Int

            init(position: IntPoint, beaconPos: IntPoint) {
                self.position = position
                self.beaconPos = beaconPos
                self.distance = position.manhattanDistance(to: beaconPos)
            }

            func getVisibleXRange(in y: Int) -> ClosedRange<Int>? {
                let deltaY = abs(position.y - y)
                let maxRange = distance - deltaY
                guard maxRange >= 0 else {
                    return nil
                }

                let minX = position.x - maxRange
                let maxX = position.x + maxRange
                return minX...maxX
            }
        }

        private var sensors: Set<Sensor> = []

        // Easy lookup of beacon/sensor positions
        private var caveMap: [IntPoint: String] = [:]

        init(input: String) {
            input
                .components(separatedBy: .newlines)
                .forEach { line in
                    let sanitizedSplit = line
                        .replacingOccurrences(of: ",", with: "")
                        .replacingOccurrences(of: ":", with: "")
                        .components(separatedBy: " ")

                    let sensorX = Int(sanitizedSplit[2].components(separatedBy: "=")[1])!
                    let sensorY = Int(sanitizedSplit[3].components(separatedBy: "=")[1])!
                    let sensorPos = IntPoint(x: sensorX, y: sensorY)

                    let beaconX = Int(sanitizedSplit[8].components(separatedBy: "=")[1])!
                    let beaconY = Int(sanitizedSplit[9].components(separatedBy: "=")[1])!
                    let beaconPos = IntPoint(x: beaconX, y: beaconY)

                    sensors.insert(Sensor(position: sensorPos, beaconPos: beaconPos))
                }

            let sensorPositions = Set(sensors.map { $0.position })
            let beaconPositions = Set(sensors.map { $0.beaconPos })

            sensorPositions.forEach { caveMap[$0] = "S" }
            beaconPositions.forEach { caveMap[$0] = "B" }
        }

        func getUnavailablePositionsCount(in y: Int) -> Int {
            let unavailablePositions = Set(sensors
                .compactMap { $0.getVisibleXRange(in: y) }
                .flatMap { $0 }
                .map { IntPoint(x: $0, y: y) }
                .filter { caveMap[$0] == nil })

            return unavailablePositions.count
        }

        func getDistressBeaconTuningFrequncy(in range: ClosedRange<Int>) -> Int {
            func isPositionUnavailable(position: IntPoint) -> Bool {
                for sensor in sensors {
                    if position.manhattanDistance(to: sensor.position) <= sensor.distance {
                        return true
                    }
                }
                return false
            }

            func getPosition() -> IntPoint {
                for sensor in sensors {
                    let offset = sensor.distance + 1

                    let top = IntPoint(x: sensor.position.x, y: sensor.position.y - offset)
                    let left = IntPoint(x: sensor.position.x - offset, y: sensor.position.y)
                    let right = IntPoint(x: sensor.position.x + offset, y: sensor.position.y)
                    let bottom = IntPoint(x: sensor.position.x, y: sensor.position.y + offset)

                    let tl = IntPoint.line(from: top, to: left)
                    let lb = IntPoint.line(from: left, to: bottom)
                    let br = IntPoint.line(from: bottom, to: right)
                    let rt = IntPoint.line(from: right, to: top)
                    let allLines = Set(tl + lb + br + rt)
                        .filter { range.contains($0.x) && range.contains($0.y) }
                        .filter { caveMap[$0] == nil }

                    for position in allLines {
                        if !isPositionUnavailable(position: position) {
                            return position
                        }
                    }
                }

                fatalError("Unable to find location")
            }

            let position: IntPoint = getPosition()
            return position.x * 4000000 + position.y
        }
    }

    override func solveFunction1() -> CustomStringConvertible {
        let input = defaultInputFileString.loadAsTextString()
        let mapper = Mapper(input: input)
        let unavailable = mapper.getUnavailablePositionsCount(in: 2000000)
        return "\(unavailable)"
    }

    override func solveFunction2() -> CustomStringConvertible {
        let input = defaultInputFileString.loadAsTextString()
        let mapper = Mapper(input: input)
        let tuningFrequency = mapper.getDistressBeaconTuningFrequncy(in: 0...4000000)
        return "\(tuningFrequency)"
    }
}
