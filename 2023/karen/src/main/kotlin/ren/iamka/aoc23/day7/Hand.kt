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

class HandComparator(val cardComparator: CardComparator = CardComparator()) : Comparator<Hand> {
    override fun compare(o1: Hand, o2: Hand): Int {
        if (o1::class != o2::class) {
            return o1.value - o2.value
        }
        o1.cards.zip(o2.cards).forEach { (o1c, o2c) ->
            val result = cardComparator.compare(o1c, o2c)
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

    val occurranceMap = validCardsPart1.associateWith { 0 }.toMutableMap()

    forEach { char ->
        if (char !in validCardsPart1) {
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

fun String.cardsToHandWithJoker(): Hand {
    if (length != 5) {
        throw IllegalArgumentException("Can't create hand of $length cards, must be 5.")
    }

    val occurranceMap = validCardsPart1.associateWith { 0 }.toMutableMap()
    val numJokers = this.count { it == 'J' }

    forEach { char ->
        if (char !in validCardsPart1) {
            throw IllegalArgumentException("Illegal card $char")
        }
        if (char != 'J') {
            occurranceMap[char] = occurranceMap[char]!! + 1
        }
    }

    if (occurranceMap.values.contains(5 - numJokers)){
        return Hand.FiveOfAKind(this)
    }

    if (occurranceMap.values.contains(4 - numJokers)){
        return Hand.FourOfAKind(this)
    }

    // 2 jokers and a pair = full house
    if (occurranceMap.values.count { it == 2 } == 1 && numJokers  == 2){
        return Hand.FullHouse(this)
    }
    // 2 pair and 1 joker = full house
    if (occurranceMap.values.count { it == 2 } == 2 && numJokers  == 1){
        return Hand.FullHouse(this)
    }

    if (occurranceMap.values.contains(3) && occurranceMap.values.contains(2)){
        return Hand.FullHouse(this)
    }

    if (occurranceMap.values.contains(3 - numJokers)){
        return Hand.ThreeOfAKind(this)
    }

    if (occurranceMap.values.count { it == 2 } == 2){
        return Hand.TwoPair(this)
    }

    if (occurranceMap.values.count { it == 2 } == 1 && numJokers > 0){
        return Hand.TwoPair(this)
    }

    if (occurranceMap.values.count { it == 2 } == 1){
        return Hand.OnePair(this)
    }

    if (numJokers > 0) {
        return Hand.OnePair(this)
    }

    return Hand.HighCard(this)
}