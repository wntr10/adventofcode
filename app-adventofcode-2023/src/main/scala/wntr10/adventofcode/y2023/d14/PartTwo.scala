package wntr10.adventofcode.y2023.d14

import wntr10.adventofcode._


object PartTwo extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  lazy val length: Int = input.length

  private def tilt(dir: Int, grid: AocGrid): AocGrid = {
    var stop = false
    var gridPrime = grid
    var rr = List.empty[AocNode]

    while (!stop) {
      stop = true
      rr = gridPrime.nodes().filter(n => n.value.str == "O").toList
      rr.foreach { r =>
        val ng = gridPrime.move(r, dir, v => v.str == ".", AocStringValue("."))
        if (ng.isDefined) {
          gridPrime = ng.get
          stop = false
        }
      }
    }
    gridPrime
  }

  private def solve(input: Parser.Alpha): BigInt = {

    val grid = AocGrid.of(input, s => AocStringValue(s))

    var gridPrime = grid

    val numberOfCycles = 1000000000

    var cycle = 0
    var stop = false

    val store = scala.collection.mutable.HashMap(gridPrime.map -> 0)
    var remainder = 0
    while (!stop && cycle < numberOfCycles) {

      gridPrime = tilt(4, gridPrime)
      gridPrime = tilt(2, gridPrime)
      gridPrime = tilt(3, gridPrime)
      gridPrime = tilt(1, gridPrime)

      cycle += 1

      if (store.contains(gridPrime.map)) {
        val start = store(gridPrime.map)
        val delta = (cycle - start)
        val todo = numberOfCycles - cycle
        remainder = todo % delta
        println("Cycle at " + cycle + " still todo " + todo + " remainder " + remainder + " ( start=" + start + " delta=" + delta + ")")
        stop = true
      } else {
        store.put(gridPrime.map, cycle)
      }
    }

    var remainderCycle = 0
    while (remainderCycle < remainder) {
      gridPrime = tilt(4, gridPrime)
      gridPrime = tilt(2, gridPrime)
      gridPrime = tilt(3, gridPrime)
      gridPrime = tilt(1, gridPrime)
      remainderCycle += 1
    }

    val rocks = gridPrime.nodes().filter(n => n.value.str == "O").toList

    rocks.map { r =>
      val xy = AocInterleave.squash(BigInt(r.key.str, 4))
      xy._2 + 1
    }.sum

  }

  println(solve(input))
}
