//
//  DeferView.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 12/11/2023.
//

import Foundation
import SwiftUI

struct DeferView<Content: View>: View {
    let content: () -> Content

    init(@ViewBuilder _ content: @escaping () -> Content) {
        self.content = content
    }
    var body: some View {
        content()
    }
}
