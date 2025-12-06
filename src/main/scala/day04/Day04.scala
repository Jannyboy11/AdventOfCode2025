package day04

import scala.io.Source

val source = Source.fromResource("day04.in")
val input: Grid = source.getLines().map(line => line.map { case '@' => Tile.Paper; case '.' => Tile.Empty }).toIndexedSeq

enum Tile:
    case Paper
    case Empty
type Grid = IndexedSeq[IndexedSeq[Tile]]
extension (grid: Grid)
    def width: Int = grid(0).size
    def height: Int = grid.size
    def apply(y: Int, x: Int): Tile = grid(y)(x)
    def apply(tup: (y: Int, x: Int)): Tile = apply(tup.y, tup.x)
    def updated(y: Int, x: Int, tile: Tile): Grid = grid.updated(y, grid(y).updated(x, tile))

def surrounding(grid: Grid, py: Int, px: Int): Seq[Tile] =
    Seq[(y: Int, x: Int)](
        (y = py-1, x = px-1), (y = py-1, x = px), (y = py-1, x = px+1),
        (y = py  , x = px-1),                     (y = py,   x = px+1),
        (y = py+1, x = px-1), (y = py+1, x = px), (y = py+1, x = px+1),
    )
        .filter((y, x) => 0 <= y && y < grid.height && 0 <= x && x < grid.width)
        .map(grid(_))

def countSurroundingPaper(grid: Grid, py: Int, px: Int): Int =
    surrounding(grid, py, px).count(_ == Tile.Paper)

def canBeLifted(grid: Grid, py:Int, px: Int): Boolean =
    grid(py, px) == Tile.Paper && countSurroundingPaper(grid, py, px) < 4

def solve1(grid: Grid): Int =
    (for y <- 0 until grid.height; x <- 0 until grid.width if canBeLifted(grid, y, x) yield ()).size

def stepRemovePaper(grid: Grid): (newGrid: Grid, removed: Int) = {
    val pointsThatCanBeLifted: Seq[(py: Int, px: Int)] = for
        y <- 0 until grid.height
        x <- 0 until grid.width
        if canBeLifted(grid, y, x)
    yield (py = y, px = x)

    if pointsThatCanBeLifted.isEmpty then
        (newGrid = grid, removed = 0)
    else
        val newGrid = pointsThatCanBeLifted.foldLeft(grid) { case (gridAcc, (py, px)) => gridAcc.updated(py, px, Tile.Empty) }
        (newGrid = newGrid, removed = pointsThatCanBeLifted.size)
}

def runRemovePaper(grid: Grid): Int =
    Seq.unfold(grid) { g =>
        val (newGrid, removed) = stepRemovePaper(g)
        if removed == 0 then None else Some((removed, newGrid))
    }.sum

@main def main(): Unit = {

    val result1 = solve1(input)
    println(result1)

    val result2 = runRemovePaper(input)
    println(result2)

}