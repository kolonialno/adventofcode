import Foundation

struct SonarSweep {

    /// count of increasing elements
    func part1(_ input: [Int]) -> Int {
        return zip(input, input.dropFirst())
            .map { $1 > $0 ? 1 : 0 }
            .reduce(0, { $0 + $1 })
    }

    /// part1 of sums of 3 slices
    func part2(_ input: [Int]) -> Int {
        let sums = slicesSums(input)

        return part1(sums)
    }

    /// sums of 3 slices
    private func slicesSums(_ input: [Int]) -> [Int] {
        var sliceStart = 0
        let sliceOffset = 2

        var sums: [Int] = []
        while sliceStart < input.count - sliceOffset {
            let slice = input[sliceStart...sliceStart+sliceOffset]
            sliceStart += 1
            sums.append(slice.reduce(0, { $0 + $1 }))
        }

        return sums
    }
}
