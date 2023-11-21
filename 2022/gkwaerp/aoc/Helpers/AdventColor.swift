//
//  AdventColor.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2022.
//

import Foundation

enum AdventColor {
    case black
    case white
    case transparent

    static var printClosure: AdventColorGrid.PrintBlock = { (value) in
        switch value {
        case .black:
            return " "
        case .white:
            return "X"
        case .transparent:
            return nil
        }
    }
}
