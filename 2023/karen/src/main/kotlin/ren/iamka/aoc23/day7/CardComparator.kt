package ren.iamka.aoc23.day7

import java.lang.IllegalArgumentException

val validCardsPart1 = listOf('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2')
val validCardsPart2 = listOf('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J')

class CardComparator(val validCards : List<Char> = validCardsPart1) : Comparator<Char> {
    override fun compare(o1: Char?, o2: Char?): Int {
        if (o1 !in validCards || o2 !in validCards) {
            throw IllegalArgumentException("Illegal cards $o1 and $o2")
        }
        return (-(validCards.indexOf(o1)) + validCards.indexOf(o2))
    }
}