//
//  DateHelper.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2022.
//

import Foundation

class DateHelper {
    static func getElapsedTimeString(from date: Date) -> String {
        let elapsedTime = Date().timeIntervalSince(date)
        return String(format: "Time = %.4f", elapsedTime)
    }
}
