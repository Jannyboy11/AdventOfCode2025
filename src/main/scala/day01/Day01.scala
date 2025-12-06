package day01

import scala.io.Source

val source = Source.fromResource("day01.in")
val input: Seq[Rotation] = source.getLines().map {
    case s"L${digits}" => Rotation.Left(digits.toInt)
    case s"R${digits}" => Rotation.Right(digits.toInt)
}.toSeq

enum Rotation:
    case Left(by: Int)
    case Right(by: Int)

def rotateLeft(angle: Int, by: Int): Int = ((angle - by) % 100 + 100) % 100
def rotateRight(angle: Int, by: Int): Int = (angle + by) % 100

def solve1(rotations: Seq[Rotation]): Int =
    rotations.scanLeft(50) {
        case (angle, Rotation.Left(by)) => rotateLeft(angle, by)
        case (angle, Rotation.Right(by)) => rotateRight(angle, by)
    }.count(_ == 0)

def encounters0Left(angle: Int, rotateBy: Int): Int = {
    val fullRounds = rotateBy / 100
    val completeRound = angle > 0 && rotateBy % 100 >= angle
    fullRounds + (if completeRound then 1 else 0)
}

def encounters0Right(angle: Int, rotateBy: Int): Int = {
    val fullRounds = rotateBy / 100
    val completeRound = rotateBy % 100 >= (100 - angle)
    fullRounds + (if completeRound then 1 else 0)
}

def solve2(rotations: Seq[Rotation]): Int =
    rotations.foldLeft((zeroEncounters = 0, angle = 50)) {
        case ((encountered, angle), Rotation.Left(by)) => (encountered + encounters0Left(angle, by), rotateLeft(angle, by))
        case ((encountered, angle), Rotation.Right(by)) => (encountered + encounters0Right(angle, by), rotateRight(angle, by))
    }.zeroEncounters

@main def main(): Unit = {

    val result1 = solve1(input)
    println(result1)

    val result2 = solve2(input);
    println(result2)

}