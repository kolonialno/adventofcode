//
//  Solver_2017_20.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 30/11/2023.
//

import Foundation

final class Solver_2017_20: Solver {
    final class Particle: Hashable {
        let index: Int
        var p: [Int]
        var a: [Int]
        var v: [Int]

        static func == (lhs: Particle, rhs: Particle) -> Bool {
            lhs.index == rhs.index
        }

        func hash(into hasher: inout Hasher) {
            hasher.combine(index)
        }

        init(_ string: String, index: Int) {
            let split = string
                .replacingOccurrences(of: "p=<", with: "")
                .replacingOccurrences(of: "v=<", with: "")
                .replacingOccurrences(of: "a=<", with: "")
                .components(separatedBy: ">, ")

            p = split[0].components(separatedBy: ",").map { $0.intValue! }
            v = split[1].components(separatedBy: ",").map { $0.intValue! }
            a = split[2].replacingOccurrences(of: ">", with: "").components(separatedBy: ",").map { $0.intValue! }
            self.index = index
        }

        func tick() {
            (0...2).forEach {
                v[$0] += a[$0]
                p[$0] += v[$0]
            }
        }
    }

    private var input: [String] = []

    override func didLoadFunction() {
        input = defaultInputFileString.loadAsStringArray()
    }

    override func solveFunction1() -> CustomStringConvertible {
        let particles = input.enumerated().map { Particle($0.element, index: $0.offset) }
        return findClosestParticleIndex(particles: particles)
    }

    override func solveFunction2() -> CustomStringConvertible {
        let particles = input.enumerated().map { Particle($0.element, index: $0.offset) }
        return getSurvivingParticleCount(particles: particles)
    }

    private func findClosestParticleIndex(particles: [Particle]) -> Int {
        (0...1_000).forEach { _ in
            particles.forEach { $0.tick() }
        }

        let closest = particles.min { p1, p2 in
            p1.p.reduce(0, { $0 + abs($1)}) < p2.p.reduce(0, { $0 + abs($1)})
        }!

        return closest.index
    }

    private func getSurvivingParticleCount(particles rawArray: [Particle]) -> Int {
        var particles = Set(rawArray)
        var areaMap: [[Int]: Set<Particle>] = [:]

        (0...1_000).forEach { _ in
            areaMap.removeAll(keepingCapacity: true)
            particles.forEach { p in
                p.tick()
                areaMap[p.p, default: []].insert(p)
            }
            let collisions = Set(areaMap.values.filter { $0.count > 1 }.flatMap { $0 })
            particles = particles.subtracting(collisions)
        }


        return particles.count
    }
}

extension Solver_2017_20: TestableDay {
    func runTests() {
        let input = """
p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>
"""
            .components(separatedBy: .newlines)

        let particles = input.enumerated().map { Particle($0.element, index: $0.offset) }
        let closest = findClosestParticleIndex(particles: particles)
        let expected = 0
        assert(closest == expected)
    }
}
