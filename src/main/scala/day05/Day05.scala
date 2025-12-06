package day05

import scala.io.Source
import scala.util.matching.Regex

val source = Source.fromResource("day05.in")
val IdRangeRegex: Regex = raw"(\d+)-(\d+)".r
val IdRegex: Regex = raw"(\d+)".r
val input: (freshIdRanges: Seq[IDRange], ids: Seq[ID]) = {
    val rangeBuilder = Seq.newBuilder[IDRange]
    val idBuilder = Seq.newBuilder[ID]
    source.getLines().foreach {
        case IdRangeRegex(lower, upper) => rangeBuilder.addOne(IDRange(lower.toLong, upper.toLong))
        case IdRegex(id) => idBuilder.addOne(id.toLong)
        case _ =>
    }
    (rangeBuilder.result(), idBuilder.result())
}

type ID = Long
case class IDRange(lower: ID, upper: ID):
    def contains(id: ID): Boolean = lower <= id && id <= upper
    def overlaps(other: IDRange) = this.contains(other.lower) || this.contains(other.upper) || other.contains(this.lower) || other.contains(this.upper)
    def length: Long = upper - lower + 1
    def merge(other: IDRange): IDRange = IDRange(Math.min(this.lower, other.lower), Math.max(this.upper, other.upper))

def solve1(input: (freshIdRanges: Seq[IDRange], ids: Seq[ID])): Int =
    input.ids.count(id => input.freshIdRanges.exists(_.contains(id)))

def mergeIDRanges(idRange: IDRange, into: Seq[IDRange]): Seq[IDRange] = into match {
    case Nil => Seq(idRange)
    case head +: tail if head.overlaps(idRange) =>
        val mergedHead = head.merge(idRange)
        if tail.nonEmpty && mergedHead.overlaps(tail.head) then
            mergedHead.merge(tail.head) +: tail.tail
        else
            mergedHead +: tail
    case head +: tail => head +: mergeIDRanges(idRange, tail)
}

def mergeIDRanges(idRanges: Seq[IDRange]): Seq[IDRange] = {
    val sortedRanges = idRanges.sortBy(_.lower)
    sortedRanges.foldLeft(Seq.empty)((res, idRange) => mergeIDRanges(idRange, res))
}

def solve2(idRanges: Seq[IDRange]): Long =
    mergeIDRanges(idRanges).map(_.length).sum

@main def main(): Unit = {

    val result1 = solve1(input)
    println(result1)

    val result2 = solve2(input.freshIdRanges)
    println(result2)

}