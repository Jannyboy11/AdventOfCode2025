package day06

import scala.:+
import scala.io.Source

val source = Source.fromResource("day06.in")
val input: Seq[String] = source.getLines().toIndexedSeq

def input1(input: Seq[String]): (numberRows: Seq[Seq[Long]], operations: Seq[Operation]) = {
    def readNumberRow(line: String): Seq[Long] = line.split("\\s+").map(_.toLong)

    val numberRows = Seq(
        readNumberRow(input(0)),
        readNumberRow(input(1)),
        readNumberRow(input(2)),
        readNumberRow(input(3)),
    )
    val operations = input(4).split("\\s+").map { case "+" => Operation.Add; case "*" => Operation.Multiply }
    (numberRows, operations)
}

enum Operation:
    case Add, Multiply

def toColumns(input: (numberRows: Seq[Seq[Long]], operations: Seq[Operation])): Seq[(numbers: Seq[Long], operation: Operation)] =
    (input.numberRows :+ input.operations)
        .transpose
        .map { case init :+ last => (init.asInstanceOf[Seq[Long]], last.asInstanceOf[Operation])}

def columnResult(numbers: Seq[Long], operation: Operation) = operation match
    case Operation.Add => numbers.sum
    case Operation.Multiply => numbers.product

def grandTotal(data: Seq[(numbers: Seq[Long], operation: Operation)]): Long =
    data.map(columnResult(_, _)).sum

def toDigit(c: Char): Int = c - '0'

def determineColumn(input: Seq[String], startX: Int): (numbers: Seq[Long], operation: Operation, columnWidth: Int) = {
    def widthAtRow(y: Int): Int = {
        val indexOfSpace = input(y).substring(startX).indexOf(' ')
        if indexOfSpace == -1 then input(0).length - startX else indexOfSpace
    }
    val columnWidth: Int = (0 until 4).map(widthAtRow).max

    def determineNumber(x: Int): Long =
        (0 until 4)
            .map(y => input(y)(x))
            .filter(c => '0' <= c && c <= '9')
            .foldLeft(0L)((acc, digitChar) => acc * 10 + toDigit(digitChar))
    val numbers: Seq[Long] = (0 until columnWidth).map(w => determineNumber(startX + w))

    val operation: Operation = input(4).charAt(startX) match { case '+' => Operation.Add; case '*' => Operation.Multiply; }

    (numbers, operation, columnWidth)
}

def input2(input: Seq[String]): Seq[(numbers: Seq[Long], operation: Operation)] = {
    val resBuilder = Seq.newBuilder[(numbers: Seq[Long], operation: Operation)]
    var x = 0
    while x < input(0).length do
        val (numbers, operation, width) = determineColumn(input, x)
        resBuilder.addOne((numbers, operation))
        x += width + 1
    resBuilder.result()
}

@main def main(): Unit = {

    val result1 = grandTotal(toColumns(input1(input)))
    println(result1)

    val result2 = grandTotal(input2(input))
    println(result2)

}