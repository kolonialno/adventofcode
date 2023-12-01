//
//  Math.swift
//  aoc
//
//  Created by Geir-Kåre S. Wærp on 29/11/2022.
//

import Foundation

class Math {
    /// Returns a value linearly interpolated from 2 floating point values and an alpha value.
    ///
    /// - Parameters:
    ///   - a: The first value.
    ///   - b: The second value.
    ///   - alpha: The alpha value.
    /// - Returns: The linearly interpolated value.
    /// If `alpha` is 0, `a` is returned.
    /// If `alpha` is 1, `b` is returned.
    /// If `alpha` is 0.25, the value 25% between `a` and `b` is returned.
    ///
    /// # Reference
    /// [Wikipedia/Linear Interpolation](https://en.wikipedia.org/wiki/Linear_interpolation)
    static func lerp<T: FloatingPoint>(a: T, b: T, alpha: T) -> T {
        (1 - alpha) * a + b * alpha
    }
}

extension Math {
    /// Returns the greatest common divisor for 2 integers.
    ///
    /// - Parameters:
    ///     - m: The first value.
    ///     - n: The second value.
    /// - Returns: The greatest common divisor for `m` and `n`.
    ///
    /// # Reference
    /// [GitHub/Swift Algorithm Club](https://github.com/raywenderlich/swift-algorithm-club/tree/master/GCD)
    static func greatestCommonDivisorIterativeEuklid(_ m: Int, _ n: Int) -> Int {
        var a: Int = 0
        var b: Int = max(m, n)
        var r: Int = min(m, n)

        while r != 0 {
            a = b
            b = r
            r = a % b
        }
        return b
    }

    /// Returns the least common multiple for 2 integers.
    ///
    /// - Parameters:
    ///     - m: The first value. Must be greater than zero.
    ///     - n: The second value. Must be greater than zero.
    /// - Returns: The least common multiple for `m` and `n`.
    /// Returns nil is either integer is not greater than zero.
    ///
    /// # Reference
    /// [GitHub/Swift Algorithm Club](https://github.com/raywenderlich/swift-algorithm-club/tree/master/GCD)
    /// Source currently has bug in LCM implementation.
    /// Issue & PR has been created, this implementation contains a version of my suggested fix.
    static func leastCommonMultiple(_ m: Int, _ n: Int) -> Int? {
        guard m > 0, n > 0 else {
            return nil
        }

        return m / greatestCommonDivisorIterativeEuklid(m, n) * n
    }
}


extension Math {
    // From StackOverflow -- https://stackoverflow.com/questions/45445699/most-efficient-way-to-find-all-the-factors-of-a-number
    static func getFactors(of n: Int) -> [Int] {
        precondition(n > 0, "n must be positive")
        let sqrtn = Int(Double(n).squareRoot())
        var factors: [Int] = []
        factors.reserveCapacity(2 * sqrtn)
        for i in 1...sqrtn {
            if n % i == 0 {
                factors.append(i)
            }
        }
        var j = factors.count - 1
        if factors[j] * factors[j] == n {
            j -= 1
        }
        while j >= 0 {
            factors.append(n / factors[j])
            j -= 1
        }
        return factors
    }
}

extension Comparable {
    /// Returns the value, restricted between minimum and maximum values.
    ///
    /// - Parameters:
    ///     - a: The first value.
    ///     - b: The second value.
    /// - Returns: The clamped value.
    /// If the original value is larger than the maximum of `a` and `b`, the maximum of `a` and `b` is returned.
    /// If the original value is smaller than the minimum of `a` and `b`, the minimum of `a` and `b` is returned.
    /// Otherwise, the original value is returned.
    func clamp(between a: Self, and b: Self) -> Self {
        let min = Swift.min(a, b)
        let max = Swift.max(a, b)
        return Swift.min(Swift.max(self, min), max)
    }

    /// Returns the value, restricted by a minimum value.
    ///
    /// - Parameters:
    ///     - min: The minimum value.
    /// - Returns: The clamped value.
    /// If the original is smaller than `min`, `min` is returned.
    /// Otherwise, the original value is returned.
    func clamp(min: Self) -> Self {
        Swift.max(min, self)
    }

    /// Returns the value, restricted by a maximum value.
    ///
    /// - Parameters:
    ///     - min: The maximum value.
    /// - Returns: The clamped value.
    /// If the original is greater than `max`, `max` is returned.
    /// Otherwise, the original value is returned.
    func clamp(max: Self) -> Self {
        Swift.min(max, self)
    }

    /// Returns the value, restricted to a closed range.
    ///
    /// - Parameters:
    ///     - range: The first closed range to clamp the value to.
    /// - Returns: The clamped value.
    /// If the original value is smaller than `range.lowerBound`, `range.lowerBound` is returned.
    /// If the original value is larger than `range.upperBound`, `range.upperBound` is returned.
    /// Otherwise, the original value is returned.
    func clamp(to range: ClosedRange<Self>) -> Self {
        clamp(between: range.lowerBound, and: range.upperBound)
    }
}
