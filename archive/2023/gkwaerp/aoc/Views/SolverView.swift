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
    let progressState: ProgressState?
    let buttonText: String
    let action: () -> Void

    var body: some View {
        VStack {
            if let progressState {
                VisualizationView(progressState: progressState)
                    .animation(.none, value: self.progressState)
            }

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
        return VStack(spacing: 20) {
            SolverView(solveType: .text,
                       solveState: .waiting,
                       progressState: nil,
                       buttonText: "Waiting",
                       action: {})
            SolverView(solveType: .text,
                       solveState: .ready,
                       progressState: nil,
                       buttonText: "Solve me",
                       action: {})
            SolverView(solveType: .text,
                       solveState: .solving,
                       progressState: nil,
                       buttonText: "Solving...",
                       action: {})
            SolverView(solveType: .text,
                       solveState: .solved(result: "Solved!"),
                       progressState: nil,
                       buttonText: "Solve me",
                       action: {})
            SolverView(solveType: .text,
                       solveState: .solving,
                       progressState: .init(text: "This is progress:\n\(imageText)",
                                            font: .system(size: 16)),
                       buttonText: "Solve me",
                       action: {})
            SolverView(solveType: .image,
                       solveState: .solved(result: imageText),
                       progressState: nil,
                       buttonText: "Solve me",
                       action: {})
        }
    }
}
