//
//  Heap+Ext.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 12/12/2022.
//

import Foundation

extension Heap where T: Equatable {
    func contains(node: T) -> Bool {
        nodes.contains(where: { $0 == node })
    }
}
