//
//  String+Ext.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2022.
//

import Foundation
import CryptoKit

extension String {
    func loadAsTextStringArray(fileType: String? = "txt", separator: String = "\n", includeEmptyLines: Bool = false) -> [String] {
        return FileLoader.loadText(fileName: self, fileType: fileType)
            .components(separatedBy: separator)
            .filter { !$0.isEmpty || includeEmptyLines }
    }

    /// Convenience, where every character is a new cell.
    func loadAsStringGrid(fileType: String? = "txt", separator: String = "\n") -> StringGrid {
        let stringArray = loadAsTextStringArray(fileType: fileType, separator: separator)
        return StringGrid(stringArray: stringArray)
    }

    func loadAsTextString(fileType: String? = "txt", trimming: Bool = true) -> String {
        return FileLoader.loadText(fileName: self, fileType: fileType, trimming: trimming)
    }

    func loadJSON<T: Codable>(fileType: String? = "txt", parseType: T.Type) -> T {
        return FileLoader.loadJSON(fileName: self, fileType: fileType, parseType: parseType)
    }

    var intValue: Int? {
        return Int(self)
    }

    /// "Character" array, to easily index into a string
    func convertToStringArray() -> [String] {
        return map { "\($0)" }
    }

    var boolValue: Bool? {
        if lowercased() == "true" {
            return true
        } else if lowercased() == "false" {
            return false
        }

        if let intValue = intValue {
            return intValue != 0
        }

        return nil
    }

    func ranges(of searchString: String) -> [Range<Index>] {
        var ranges: [Range<Index>] = []
        var startIndex = self.startIndex

        while startIndex != endIndex {
            let range = startIndex..<endIndex

            if let foundRange = self.range(of: searchString, range: range) {
                ranges.append(foundRange)
            } else {
                break
            }

            startIndex = index(after: startIndex)
        }

        return ranges
    }
}

extension String {
    var md5AsHex: String {
        let digest = Insecure.MD5.hash(data: data(using: .utf8) ?? Data())

        return digest.map {
            String(format: "%02hhx", $0)
        }.joined()
    }
}

extension String {
    static func yearAndDayString(year: Int, day: Int, prefix: String) -> String {
        String(format: "%@_%04d_%02d", prefix, year, day)
    }
}
