//
//  VisualizationView.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 14/12/2022.
//

import Foundation
import SwiftUI

struct VisualizationView: View {
    let progressState: ProgressState

    var body: some View {
        Text(progressState.text)
            .font(progressState.font)
    }
}
