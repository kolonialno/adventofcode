//
//  YearView.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 12/11/2023.
//

import Foundation
import SwiftUI

struct YearView: View {
    let years: [Int]

    var body: some View {
        NavigationStack {
            listView
                .navigationDestination(for: Int.self) { year in
                    CalendarView(year: year)
                }
        }
    }

    private var listView: some View {
        List {
            ForEach(years, id: \.self) { year in
                NavigationLink {
                    CalendarView(year: year)
                } label: {
                    Text(String(year))
                }
            }
        }.navigationTitle("Advent of Code")
    }
}

struct YearView_Previews: PreviewProvider {
    static var previews: some View {
        YearView(years: [2017, 2022])
    }
}
