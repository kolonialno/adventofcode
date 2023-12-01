//
//  Solver_2022_07.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 07/12/2022.
//

import Foundation

class Solver_2022_07: Solver {
    enum Operation {
        case cd(path: String)
        case ls(result: [String])

        static func from(string: String) -> [Operation] {
            return string.components(separatedBy: "$ ")
                .filter { !$0.isEmpty }
                .map { $0.trimmingCharacters(in: .newlines) }
                .map { substring in
                    if substring.hasPrefix("cd") {
                        let subsplit = substring.components(separatedBy: " ")
                        return .cd(path: subsplit[1])
                    } else if substring.hasPrefix("ls") {
                        let subsplit = Array(substring.components(separatedBy: .newlines).dropFirst())
                        return .ls(result: subsplit)
                    } else {
                        fatalError()
                    }
                }
        }
    }

    class FileManager {
        /// File path --> Directory
        var directories: [String: Directory]

        var paths: [String]

        var currentPath: String {
            return paths.joined(separator: "/")
        }

        var currentDirectory: Directory {
            return directories[currentPath]!
        }

        private func createDirectoryIfNeeded(with directoryName: String) {
            let newPath = [currentPath, directoryName].joined(separator: "/")

            guard !currentDirectory.directoryPaths.contains(newPath) else {
                return
            }

            currentDirectory.directoryPaths.insert(newPath)
            directories[newPath] = .init()
        }

        class File {
            let filename: String
            let size: Int

            init(filename: String, size: Int) {
                self.filename = filename
                self.size = size
            }
        }

        class Directory {
            var files: [File]
            var directoryPaths: Set<String>

            init() {
                self.files = []
                self.directoryPaths = []
            }
        }

        private func getFileSize(for directoryPath: String) -> Int {
            let directory = directories[directoryPath]!
            let fileSize = directory.files
                .map { $0.size }
                .reduce(0, +)
            let directorySize = directory.directoryPaths
                .map { getFileSize(for: $0) }
                .reduce(0, +)

            return fileSize + directorySize
        }

        private func handle(operation: Operation) {
            switch operation {
            case .cd(let path):
                if path == "/" {
                    paths = ["/"]
                } else if path == ".." {
                    _ = paths.popLast()
                } else {
                    createDirectoryIfNeeded(with: path)
                    paths.append(path)
                }
            case .ls(let result):
                result.forEach { string in
                    let split = string.components(separatedBy: " ")
                    if split[0] == "dir" {
                        let path = split[1]
                        createDirectoryIfNeeded(with: path)
                    } else {
                        let size = Int(split[0])!
                        currentDirectory.files.append(.init(filename: split[1], size: size))
                    }
                }
            }
        }

        init() {
            self.directories = ["/": .init()]
            self.paths = ["/"]
        }

        func handle(operations: [Operation]) {
            operations.forEach { handle(operation: $0) }
        }

        func getTotalFilteredSize(maximumFileSize: Int) -> Int {
            directories.keys
                .map { getFileSize(for: $0) }
                .filter { $0 <= maximumFileSize }
                .reduce(0, +)
        }

        func getSizeOfFolderToDelete() -> Int {
            let totalSpace = 70_000_000
            let updateSize = 30_000_000
            let unusedSpace = totalSpace - getFileSize(for: "/")
            let requiredSpace = updateSize - unusedSpace

            return directories.keys
                .map { getFileSize(for: $0) }
                .filter { $0 >= requiredSpace }
                .min()!
        }
    }

    private var fileManager = FileManager()

    override func didLoadFunction() {
        let operations = Operation.from(string: defaultInputFileString.loadAsTextString())
        fileManager.handle(operations: operations)
    }

    override func solveFunction1() -> CustomStringConvertible {
        let result = fileManager.getTotalFilteredSize(maximumFileSize: 100_000)
        return "\(result)"
    }

    override func solveFunction2() -> CustomStringConvertible {
        let result = fileManager.getSizeOfFolderToDelete()
        return "\(result)"
    }
}

extension Solver_2022_07: TestableDay {
    func runTests() {
        let testInput =
"""
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
"""
        let fileManager = FileManager()
        let operations = Operation.from(string: testInput)

        fileManager.handle(operations: operations)

        let filteredSize = fileManager.getTotalFilteredSize(maximumFileSize: 100_000)
        assert(filteredSize == 95_437)

        let deletingFolderSize = fileManager.getSizeOfFolderToDelete()
        assert(deletingFolderSize == 24_933_642)
    }
}
