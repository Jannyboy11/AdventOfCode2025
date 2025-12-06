package day03

import scala.io.Source

val source = Source.fromResource("day03.in")
val input: Seq[Bank] = source.getLines().toSeq

type Bank = String

def toDigit(c: Char): Int = c - '0'
def joltage(chars: Char*): Long = chars.foldLeft(0L)((total, acc) => total * 10 + toDigit(acc))
def joltage(bank: Bank, indexes: Int*): Long = joltage(indexes.map(bank)*)

def findMaxJoltage(bank: Bank, numBatteries: Int): Long = {
    val indexes: Seq[Int] = Range.apply(0, numBatteries)
        .scanLeft((startSearchIdx = 0, foundIdx = 0)) { case ((start, found), batteryNo) =>
            val maxIdx: Int = findMaxJoltageIndex(bank, start, bank.length - numBatteries + 1 + batteryNo)
            (maxIdx + 1, maxIdx)
        }
        .tail // omit initial state
        .map(_.foundIdx)
    joltage(bank, indexes*)
}

def findMaxJoltageIndex(bank: Bank, start: Int, endExclusive: Int): Int =
    bank.substring(start, endExclusive).zipWithIndex.maxBy((battery, index) => battery)._2 + start

def solve(input: Seq[Bank], numBatteries: Int): Long =
    input.map(findMaxJoltage(_, numBatteries)).sum

@main def main(): Unit = {

    val result1 = solve(input, 2)
    println(result1)

    val result2 = solve(input, 12)
    println(result2)

}