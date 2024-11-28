//
//  GridInfo.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2022.
//

import Foundation

struct GridInfo {
    let minExtents: IntPoint
    let maxExtents: IntPoint

    var width: Int {
        maxExtents.x - minExtents.x + 1
    }

    var height: Int {
        maxExtents.y - minExtents.y + 1
    }

    static var zero: GridInfo {
        GridInfo(minExtents: .origin, maxExtents: .origin)
    }

    var allPoints: [IntPoint] {
        let rawPoints = IntPoint(x: self.width, y: self.height).gridPoints
        return rawPoints.map({minExtents + $0})
    }
}
