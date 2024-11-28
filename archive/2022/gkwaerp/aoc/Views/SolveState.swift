//
//  SolveState.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 30/11/2022.
//

import Foundation

enum SolveState: Equatable {
    case waiting
    case ready
    case solving
    case solved(result: String)
}

extension SolveState {
    var progressViewOpacity: Double {
        switch self {
        case .solving:
            return 1
        case .waiting, .ready, .solved:
            return 0
        }
    }

    var textOpacity: Double {
        switch self {
        case .solved:
            return 1
        case .waiting, .ready, .solving:
            return 0
        }
    }

    var buttonOpacity: Double {
        switch self {
        case .waiting, .ready:
            return 1
        case .solving, .solved:
            return 0
        }
    }

    var buttonIsDisabled: Bool {
        switch self {
        case .waiting, .solving, .solved:
            return true
        case .ready:
            return false
        }
    }

    var text: String {
        switch self {
        case .waiting, .ready, .solving:
            return ""
        case .solved(let result):
            return result
        }
    }
}
