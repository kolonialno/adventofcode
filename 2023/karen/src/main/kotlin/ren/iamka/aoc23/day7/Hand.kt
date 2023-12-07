package ren.iamka.aoc23.day7

import java.lang.IllegalArgumentException

sealed class Hand(val value: Int) {
    abstract val cards: String

    data class FiveOfAKind(override val cards: String) : Hand(value = 7)
    data class FourOfAKind(override val cards: String) : Hand(value = 6)
    data class FullHouse(override val cards: String) : Hand(value = 5)
    data class ThreeOfAKind(override val cards: String) : Hand(value = 4)
    data class TwoPair(override val cards: String) : Hand(value = 3)
    data class OnePair(override val cards: String) : Hand(value = 2)
    data class HighCard(override val cards: String) : Hand(value = 1)
}

class HandComparator : Comparator<Hand> {
    override fun compare(o1: Hand, o2: Hand): Int {
        if (o1::class != o2::class) {
            return o1.value - o2.value
        }
        val comparator = CardComparator()
        o1.cards.zip(o2.cards).forEach { (o1c, o2c) ->
            val result = comparator.compare(o1c, o2c)
            if (result != 0) {
                return result
            }
        }
        return 0
    }
}

fun String.cardsToHand(): Hand {
    if (length != 5) {
        throw IllegalArgumentException("Can't create hand of $length cards, must be 5.")
    }

    val occurranceMap = validCards.associateWith { 0 }.toMutableMap()

    forEach { char ->
        if (char !in validCards) {
            throw IllegalArgumentException("Illegal card $char")
        }
        occurranceMap[char] = occurranceMap[char]!! + 1
    }

    if (occurranceMap.values.contains(5)){
        return Hand.FiveOfAKind(this)
    }

    if (occurranceMap.values.contains(4)){
        return Hand.FourOfAKind(this)
    }

    if (occurranceMap.values.contains(3) && occurranceMap.values.contains(2)){
        return Hand.FullHouse(this)
    }

    if (occurranceMap.values.contains(3)){
        return Hand.ThreeOfAKind(this)
    }

    if (occurranceMap.values.count { it == 2 } == 2){
        return Hand.TwoPair(this)
    }

    if (occurranceMap.values.count { it == 2 } == 1){
        return Hand.OnePair(this)
    }

    return Hand.HighCard(this)
}