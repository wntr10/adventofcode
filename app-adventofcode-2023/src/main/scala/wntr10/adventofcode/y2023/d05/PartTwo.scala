package wntr10.adventofcode.y2023.d05

import wntr10.adventofcode.{AocRange, Input}

import java.util.concurrent.TimeUnit


object PartTwo extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  private def help(value: AocRange, ranges: List[(AocRange, AocRange)]): Set[AocRange] = {
    var todo = Set(value)
    var prime = Set.empty[AocRange]
    ranges.foreach { rm =>
      val r = rm._1
      prime = prime ++ rm._2(r.intersectIdx(todo))
      todo = todo.flatMap(t => t.minus(r))
    }
    prime ++ todo
  }

  private def mapRanges(values: List[AocRange], ranges: List[(AocRange, AocRange)]): List[AocRange] = {
    values.flatMap { value =>
      help(value, ranges).filter(r => r.length != 0)
    }
  }

  private def solve(input: Parser.Alpha): Long = {

    val it = input(0).b.toList.head.iterator

    val start = System.nanoTime()

    var seedRanges = List.empty[AocRange]

    while (it.hasNext) {
      val start = it.next()
      val length = it.next()
      seedRanges = AocRange.of(start, length) :: seedRanges
    }

    seedRanges = seedRanges.reverse

    var list: List[AocRange] = seedRanges

    Range.inclusive(1, 7).foreach { step =>
      val ranges = input(step).b.map { l =>
        (AocRange.of(l(1), l(2)), AocRange.of(l(0), l(2)))
      }.toList
      list = mapRanges(list, ranges)
    }

    val solution = list.map(r => r.start).min
    println(TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start) + "ms")
    solution
  }

  println(solve(input))

}
