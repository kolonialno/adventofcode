//
//  SolverView.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 30/11/2022.
//

import Foundation
import SwiftUI

// MARK: - UI
struct SolverView: View {
    let solveState: SolveState
    let buttonText: String
    let action: () -> Void

    var body: some View {
        ZStack {
            ProgressView()
                .opacity(solveState.progressViewOpacity)

            Text(solveState.text)
                .opacity(solveState.textOpacity)

            Button(action: action, label: {
                Text(buttonText)
            })
            .opacity(solveState.buttonOpacity)
            .disabled(solveState.buttonIsDisabled)
        }
        .animation(.default, value: solveState)
    }
}

// MARK: - Previews
struct SolverView_Previews: PreviewProvider {
    static var previews: some View {
        VStack {
            SolverView(solveState: .waiting, buttonText: "Waiting", action: {})
            SolverView(solveState: .ready, buttonText: "Solve me", action: {})
            SolverView(solveState: .solving, buttonText: "Solving...", action: {})
            SolverView(solveState: .solved(result: "Solved!"), buttonText: "Solve me", action: {})
        }
    }
}
