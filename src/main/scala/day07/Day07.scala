package day07

import scala.collection.mutable
import scala.io.Source

val source = Source.fromResource("day07.in")
val input: Grid = source.getLines().map(_.map {
    case '.' => Tile.Empty
    case 'S' => Tile.Start
    case '^' => Tile.Splitter
}.toIndexedSeq).toIndexedSeq

enum Tile:
    case Empty, Start, Splitter
type Grid = IndexedSeq[IndexedSeq[Tile]]
type X = Int
type Y = Int

def findStartX(grid: Grid): X = grid.head.indexWhere(_ == Tile.Start)

def findSplitters(row: IndexedSeq[Tile]): Seq[X] =
    row.zipWithIndex.filter((tile, _) => tile == Tile.Splitter).map(_._2)

def stepGrid(grid: Grid, y: Y, beams: Set[X]): (newBeams: Set[X], splits: Int) = {
    val splitterIndexes = findSplitters(grid(y))
    val intersections = beams.intersect(splitterIndexes.toSet)
    val splitBeams = intersections.flatMap(x => Set(x-1, x+1))
    val newBeams = beams -- intersections ++ splitBeams
    (newBeams, intersections.size)
}

def solve1(input: Grid, startX: X): Int =
    input.indices.foldLeft((count = 0, beams = Set(startX))) { case ((count, beams), y) =>
        val (newBeams, splits) = stepGrid(input, y, beams)
        (count = count + splits, beams = newBeams)
    }.count

def timelines(grid: Grid, x: X, y: Y, cache: mutable.Map[(X, Y), Long]): Long =
    cache.getOrElseUpdate((x, y), if y == grid.size then 1L
    else if grid(y)(x) == Tile.Splitter then timelines(grid, x-1, y+1, cache) + timelines(grid, x+1, y+1, cache)
    else timelines(grid, x, y+1, cache))

def solve2(input: Grid, startX: X): Long =
    timelines(input, startX, 0, new mutable.HashMap[(X, Y), Long]())

@main def main(): Unit = {

    val startX: Int = findStartX(input)

    val result1 = solve1(input, startX)
    println(result1)

    val result2 = solve2(input, startX)
    println(result2)

}