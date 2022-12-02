//
//  DayView.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2022.
//

import Foundation
import SwiftUI

// MARK: - UI
struct DayView: View {
    @ObservedObject var solver: Solver

    var body: some View {
        GeometryReader { geometry in
            VStack {
                SolverView(solveState: solver.solveState1, buttonText: "Solve Part 1") {
                    solver.solvePart1()
                }
                .frame(width: geometry.size.width, height: geometry.size.height / 2)

                SolverView(solveState: solver.solveState2, buttonText: "Solve Part 2") {
                    solver.solvePart2()
                }
                .frame(width: geometry.size.width, height: geometry.size.height / 2)
            }
        }
        .padding()
        .navigationTitle(solver.navigationTitle)
        .onAppear {
            solver.prepareForSolve()
        }
    }
}

// MARK: - Previews
struct DayView_Previews: PreviewProvider {
    static var previews: some View {
        DayView(solver: DayView.MockSolver(year: 2022, day: 15))
    }
}

// MARK: - Mock
private extension DayView {
    class MockSolver: Solver {
        override func solveFunction1() -> String {
            return "Julegrøt"
        }

        override func solveFunction2() -> String {
            return "Gløgg"
        }
    }
}
