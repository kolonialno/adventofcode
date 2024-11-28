//
//  NoSpaceLeftOnDevice.swift
//  aoc2022
//
//  Created by Yuliia Veresklia on 07/12/2022.
//

import Foundation

struct NoSpaceLeftOnDevice {

    private func fileSystem(from input: String) -> [[String]: Int] {
        let lines = input.components(separatedBy: "\n")

        var fileSystem = [[String]: Int]()
        fileSystem[["/"]] = 0

        var currentDirPath: [String] = []
        for line in lines {
            if line.hasPrefix("$ cd ") {
                let dirName = line.components(separatedBy: " ")[2]
                switch dirName {
                case "..":
                    currentDirPath = currentDirPath.dropLast(1)
                default:
                    currentDirPath.append(line.components(separatedBy: " ")[2])
                }

            } else if line.hasPrefix("$ ls") {
                print("just listing")
            } else {
                let comps = line.components(separatedBy: " ")
                switch comps[0] {
                case "dir":
                    print("just a directory")
                default:
                    let size = Int(comps[0])!

                    var parents = currentDirPath
                    while !parents.isEmpty {
                        let current = fileSystem[parents] ?? 0
                        fileSystem[parents] = current + size
                        parents = parents.dropLast(1)
                    }
                }
            }
        }

        return fileSystem
    }

    func part1(_ input: String) -> Int {
        return fileSystem(from: input)
            .filter { $0.value <= 100000 }
            .map { $0.value }
            .reduce(0, +)
    }

    func part2(_ input: String) -> Int {
        let fileSystem = fileSystem(from: input)
        let takenSpace = fileSystem[["/"]]!
        let freeSpace = 70000000 - takenSpace
        let toDelete = 30000000 - freeSpace

        return fileSystem
            .filter { $0.value >= toDelete }
            .map { $0.value }
            .min()!
    }
}
