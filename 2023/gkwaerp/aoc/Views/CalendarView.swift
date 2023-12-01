//
//  CalendarView.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2022.
//

import SwiftUI

// MARK: - UI
struct CalendarView: View {
    let year: Int

    private let numColumns = 6
    private let numRows = 4

    /// Day --> Solver
    private typealias SolverDictionary = [Int: Solver]
    @State private var solvers: SolverDictionary = [:]

    var body: some View {
        NavigationStack {
            grid
            .navigationTitle("Advent of Code, \(String(year))")
        }
        .onAppear {
            updateDays()
        }
    }

    private var grid: some View {
        return Grid(alignment: .center, horizontalSpacing: 8, verticalSpacing: 8) {
            ForEach(0..<numColumns, id: \.self) { column in
                GridRow(alignment: .center) {
                    ForEach(0..<numRows, id: \.self) { row in
                        let day = column * numRows + row + 1
                        button(for: day)
                    }
                }
            }

            GridRow {
                button(for: 25)
                    .gridCellColumns(4)
            }
        }
    }

    private func button(for day: Int) -> some View {
        let isDisabled = solvers[day] == nil

        return NavigationLink {
            DeferView {
                let solver = solvers[day]!
                DayView(solver: solver)
            }
        } label: {
            Text(day.toDayString())
                .padding()
                .background(isDisabled ? Color.gray : Color.green)
                .cornerRadius(8)
        }.disabled(isDisabled)
    }

    // MARK: Population Logic
    private func updateDays() {
        guard solvers.isEmpty else {
            return
        }

        for day in 1...25 {
            solvers[day] = Solver.getType(year: year, day:  day)?.init(year: year, day: day)
        }
    }
}

// MARK: - Previews
struct CalendarView_Previews: PreviewProvider {
    static var previews: some View {
        CalendarView(year: 2022)
    }
}
