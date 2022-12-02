//
//  Node.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2022.
//

import Foundation

protocol Node {
    var children: [Node] { set get }
    var parent: Node? { set get }
}

extension Node {
    var allParents: [Node] {
        var parents = [Node]()
        var currNode = parent

        while let actualNode = currNode {
            parents.append(actualNode)
            currNode = actualNode.parent
        }

        return parents
    }

    var allChildren: [Node] {
        var allChildren = [Node]()

        for child in children {
            allChildren.append(contentsOf: child.allChildren)
        }

        return allChildren
    }
}
