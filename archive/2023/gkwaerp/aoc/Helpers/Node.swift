//
//  Node.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2022.
//

import Foundation

class TreeNode<T> {
    var value: T
    var children: [TreeNode]
    var parent: TreeNode?

    init(value: T, children: [TreeNode], parent: TreeNode? = nil) {
        self.value = value
        self.children = children
        self.parent = parent
    }
}

extension TreeNode {
    var allParents: [TreeNode] {
        var parents = [TreeNode]()
        var currNode = parent

        while let actualNode = currNode {
            parents.append(actualNode)
            currNode = actualNode.parent
        }

        return parents
    }

    var allChildren: [TreeNode] {
        var allChildren = children

        for child in children {
            allChildren.append(contentsOf: child.allChildren)
        }

        return allChildren
    }
}
