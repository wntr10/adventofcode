package wntr10.adventofcode.y2023.d11

import wntr10.adventofcode._

object PartTwo extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  lazy val length: Int = input.length

  private def expand(set: Set[Int], factor: BigInt)(v: BigInt): BigInt = {
    val p = if (set.contains(v.toInt)) {
      factor
    } else {
      BigInt(1)
    }
    p
  }

  private def solve(input: Parser.Alpha): BigInt = {

    val exx = Range(0, input(0).length).flatMap { c =>
      if (input.map { line =>
        line(c)
      }.forall(r => r == ".")) {
        Set(c)
      } else {
        Set.empty
      }
    }.toSet

    println(exx)

    val exy = input.zipWithIndex.filter(l => l._1.forall(c => c == ".")).map(k => (input.length - k._2) - 1).toSet

    println(exy)

    val grid = AocGrid.of(input, s => AocStringValue(s))
    val subGrid = grid
    subGrid.show()

    val current = grid.nodes.filter(n => n.value.str == "#").toList

    var ll = List.empty[(AocNode, AocNode)]

    current.foreach { start =>
      current.foreach { end =>
        if (start != end && !ll.contains((end, start))) {
          ll = (start, end) :: ll
        }
      }
    }

    println("number of pairs " + ll.size)

    var num = 0

    val e1: BigInt => BigInt = expand(exx, BigInt(1000000))
    val e2: BigInt => BigInt = expand(exy, BigInt(1000000))

    ll.map { pair =>
      num += 1
      grid.manhattan2(pair._1, pair._2, e1, e2)
    }.sum

  }

  println(solve(input))
}
