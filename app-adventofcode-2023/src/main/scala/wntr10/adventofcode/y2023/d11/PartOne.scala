package wntr10.adventofcode.y2023.d11

import wntr10.adventofcode._


object PartOne extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  lazy val length: Int = input.length

  private def solve(input: Parser.Alpha): Long = {
    val expandX = Range(0, input(0).length).flatMap { c =>
      if (input.map { line =>
        line(c)
      }.forall(r => r == ".")) {
        Set(c)
      } else {
        Set.empty
      }
    }.toSet

    val inputX = input.flatMap { line =>
      val lineTmp = line.zipWithIndex.flatMap(cz =>
        if (expandX.contains(cz._2)) {
          List(cz._1, cz._1)
        } else {
          List(cz._1)
        }
      )

      if (line.forall(c => c == ".")) {
        List(lineTmp, lineTmp)
      } else {
        List(lineTmp)
      }
    }

    val grid = AocGrid.of(inputX, s => AocStringValue(s))

    val current = grid.nodes().filter(n => n.value.str == "#").toList

    val pairs = scala.collection.mutable.Set.empty[(AocNode, AocNode)]

    current.foreach { start =>
      current.foreach { end =>
        if (start != end) {
          pairs.add((start, end))
        }
      }
    }

    println("Number of pairs " + pairs.size)

    pairs.map { pair =>
      grid.manhattan(pair._1, pair._2).toInt
    }.sum

  }

  println(solve(input))
}
