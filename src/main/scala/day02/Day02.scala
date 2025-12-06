package day02

import scala.io.Source

val source = Source.fromResource("day02.in")
val input: Seq[IDRange] = source.getLines().next().split(",").map { case s"${n1}-${n2}" => IDRange(n1.toLong, n2.toLong) }

case class IDRange(lowerBound: Long, upperBound: Long)

def enumerateInvalidIds(lower: Long, upper: Long, isInvalid: Long => Boolean): Seq[Long] = {
    val magLower = magnitude(lower)
    val magUpper = magnitude(upper)
    LazyList.iterate(lower)(_ + 1).filter(isInvalid).takeWhile(_ <= upper)
}

def isInvalid1(id: Long): Boolean = {
    val halfMag = magnitude(id) / 2
    val orderOfMagnitude = Math.pow(10, halfMag).toLong
    val leastSignificantPart = id % orderOfMagnitude
    val mostSignificantPart = (id - leastSignificantPart) / orderOfMagnitude
    leastSignificantPart == mostSignificantPart
}

def isInvalid2(id: Long): Boolean =
    LazyList.range(1, magnitude(id) / 2 + 1).exists(range => isInvalid2(id, range))

def isInvalid2(id: Long, range: Int): Boolean = {
    val string = id.toString
    val substring = string.substring(0, range)
    val times = string.length / range
    string == substring.repeat(times)
}

def magnitude(value: Long): Int =
    if value >= 10 then 1 + magnitude(value / 10) else 1

def solve(input: Seq[IDRange], isInvalid: Long => Boolean): Long = input
    .flatMap { case IDRange(lower, upper) => enumerateInvalidIds(lower, upper, isInvalid) }
    .sum

@main def main(): Unit = {

    val result1 = solve(input, isInvalid1)
    println(result1)

    val result2 = solve(input, isInvalid2)
    println(result2)

}