//
//  FileLoader.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2022.
//

import Foundation

class FileLoader {
    private static func getData(for fileName: String, fileType: String?) -> Data {
        try! Data(contentsOf: getURL(for: fileName, fileType: fileType))
    }

    private static func getURL(for fileName: String, fileType: String?) -> URL {
        let path = Bundle.main.path(forResource: fileName, ofType: fileType)!
        return URL(fileURLWithPath: path)
    }

    static func loadJSON<T: Codable>(fileName: String, fileType: String? = nil, parseType: T.Type) -> T {
        let data = getData(for: fileName, fileType: fileType)
        return try! JSONDecoder().decode(T.self, from: data)
    }

    static func loadText(fileName: String, fileType: String? = nil, trimming: Bool = true) -> String {
        let string = try! String(contentsOf: getURL(for: fileName, fileType: fileType), encoding: .utf8)
        return trimming ? string.trimmingCharacters(in: .whitespacesAndNewlines) : string
    }
}
