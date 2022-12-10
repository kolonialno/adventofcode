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
    enum SolveType {
        case text
        case image

        var font: Font {
            switch self {
            case .text:
                return .body
            case .image:
                return .system(size: 4)
            }
        }
    }

    let solveType: SolveType
    let solveState: SolveState
    let buttonText: String
    let action: () -> Void

    var body: some View {
        ZStack {
            ProgressView()
                .opacity(solveState.progressViewOpacity)

            Text(solveState.text)
                .font(solveType.font)
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
        let imageText = """
🟦🟦🟧🟧🟦🟦🟧🟧🟧🟦🟦
🟦🟧🟦🟦🟧🟦🟧🟦🟦🟧🟦
🟦🟧🟦🟦🟧🟦🟧🟧🟧🟦🟦
🟦🟧🟧🟧🟧🟦🟧🟦🟦🟧🟦
🟦🟧🟦🟦🟧🟦🟧🟦🟦🟧🟦
🟦🟧🟦🟦🟧🟦🟧🟧🟧🟦🟦
"""
        return VStack {
            SolverView(solveType: .text, solveState: .waiting, buttonText: "Waiting", action: {})
            SolverView(solveType: .text, solveState: .ready, buttonText: "Solve me", action: {})
            SolverView(solveType: .text, solveState: .solving, buttonText: "Solving...", action: {})
            SolverView(solveType: .text, solveState: .solved(result: "Solved!"), buttonText: "Solve me", action: {})
            SolverView(solveType: .image, solveState: .solved(result: imageText), buttonText: "Solve me", action: {})
        }
    }
}
