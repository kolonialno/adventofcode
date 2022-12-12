//
//  PriorityQueue+Ext.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 12/12/2022.
//

import Foundation

extension PriorityQueue where T: Equatable {
    func contains(node: T) -> Bool {
        heap.contains(node: node)
    }
}
