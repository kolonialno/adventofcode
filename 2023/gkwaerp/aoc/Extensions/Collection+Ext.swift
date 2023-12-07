//
//  Collection+Ext.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 07/12/2023.
//

import Foundation

extension Collection {
    var isNotEmpty: Bool {
        !isEmpty
    }
}

extension Collection where Element: Hashable {
    func uniqueElements() -> [Element] {
        var seen: Set<Element> = []
        return self.filter { seen.insert($0).inserted == true }
    }
}
