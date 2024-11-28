//
//  Common_2017.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 26/11/2023.
//

import Foundation

enum Common_2017 {
    final class KnotHasher {
        let listSize: Int
        var skipSize: Int
        let inputLengths: [Int]
        var list: [Int]
        var currentIndex: Int

        init(listSize: Int, inputLengths: [Int]) {
            self.listSize = listSize
            self.skipSize = 0
            self.inputLengths = inputLengths
            self.list = (0..<listSize).map { $0 }
            self.currentIndex = 0
        }

        init(listSize: Int, string: String) {
            self.listSize = listSize
            self.skipSize = 0
            self.inputLengths = string.map { Int($0.asciiValue!) } + [17, 31, 73, 47, 23]
            self.list = (0..<listSize).map { $0 }
            self.currentIndex = 0
        }

        func runHash(numRounds: Int = 1) {
            (0..<numRounds).forEach { _ in
                inputLengths.forEach { inputLength in
                    guard inputLength < listSize else { return }

                    var reversedSublist = [Int]()
                    for i in 0..<inputLength {
                        let index = (currentIndex + i) % listSize
                        reversedSublist.append(list[index])
                    }
                    reversedSublist.reverse()

                    for i in 0..<inputLength {
                        let index = (currentIndex + i) % listSize
                        list[index] = reversedSublist[i]
                    }

                    currentIndex += inputLength + skipSize
                    currentIndex %= listSize
                    skipSize += 1
                }
            }
        }

        func getHashValue() -> Int {
            return list[0] * list[1]
        }

        private func getDenseHash() -> [Int] {
            var denseHash = [Int]()
            stride(from: 0, through: listSize - 16, by: 16).forEach { blockStartIndex in
                let output = (blockStartIndex..<(blockStartIndex + 16)).reduce(0, { $0 ^ list[$1] })
                denseHash.append(output)
            }
            return denseHash
        }

        func getHexString() -> String {
            let denseHash = getDenseHash()
            return denseHash.map { String(format: "%02x", $0) }.joined()
        }
    }
}
