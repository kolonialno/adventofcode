//
//  aocApp.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2022.
//

import SwiftUI

@main
struct aocApp: App {
    var body: some Scene {
        let currentYear = Calendar.current.dateComponents([.year], from: .now).year!
        let years = Array(2015...currentYear)
        WindowGroup {
            YearView(years: years)
        }
    }
}
