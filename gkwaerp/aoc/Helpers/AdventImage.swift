//
//  AdventImage.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2022.
//

import Foundation

class AdventImage {
    var size: IntPoint
    var layers: [AdventColorGrid]

    var width: Int {
        size.x
    }

    var height: Int {
        size.y
    }

    init(size: IntPoint, values: [AdventColor]) {
        let valuesPerLayer = size.x * size.y
        let numLayers = values.count / valuesPerLayer
        var valuesToUse = values
        var layers = [AdventColorGrid]()
        for _ in 0..<numLayers {
            let layerData = Array(valuesToUse.prefix(valuesPerLayer))
            valuesToUse = Array(valuesToUse.dropFirst(valuesPerLayer))
            let grid = AdventColorGrid(size: size, values: layerData)
            layers.append(grid)
        }

        self.size = size
        self.layers = layers
    }

//    func getLayerIndexWithFewestMatching(color: Color) -> Int {
//        var fewestSoFar = self.width * self.height
//        var bestIndex = -1
//        for (index, layer) in self.layers.enumerated() {
//            let matchingInLayer = layer.getNumPixels(matching: color)
//            if matchingInLayer < fewestSoFar {
//                fewestSoFar = matchingInLayer
//                bestIndex = index
//            }
//        }
//        return bestIndex
//    }
//    func getNumMatchingPixelsInLayer(layerIndex: Int, color: Color) -> Int {
//        return self.layers[layerIndex].getNumPixels(matching: color)
//    }
//    lazy var rasterized: Layer = {
//        var rasterizedData = [Color]()
//        for y in 0..<self.height {
//            for x in 0..<self.width {
//                rasterizedData.append(self.findPixel(x: x, y: y)!)
//            }
//        }
//        return Layer(width: self.width, height: self.height, pixels: rasterizedData)
//    }()
//    private func findPixel(x: Int, y: Int) -> Color? {
//        for layer in self.layers {
//            let color = layer.getPixel(x: x, y: y)
//            switch color {
//            case .transparent:
//                break
//            default:
//                return color
//            }
//        }
//
//        return nil
//    }
}
