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
    var todo = value
    var prime = Set.empty[AocRange]
    ranges.foreach { rm =>
      val r = rm._1
      if (r.before(todo)) {
        // skip
      } else if (r.after(todo)) {
        return prime + todo
      } else if (r.equals(todo)) {
        return prime + rm._2
      } else if (r.starts(todo)) {
        todo = todo.rsubrange(todo.length - r.length)
        prime += rm._2
      } else if (r.finishes(todo)) {
        val tmp = r.minus(todo).head.length
        val tmp2 = rm._2.subrange(tmp)
        return prime + tmp2
      } else if (r.during(todo)) {
        return prime ++ todo.minus(r) + rm._2
      } else if (todo.during(r)) {
        val idx = Set(r.intersectIdx(todo))
        return prime ++ rm._2(idx)
      } else if (todo.overlap(r)) {
        val tmp = todo.length
        todo = todo.minus(r).head
        return prime + todo + rm._2.rsubrange(r.length - (tmp - todo.length))
      } else if (r.overlap(todo)) {
        val tmp = r.minus(todo).head.length
        val tmp2 = rm._2.subrange(tmp)
        todo = todo.subrange(tmp2.length)
        prime = prime + tmp2
      } else {
        throw new RuntimeException()
      }
    }
    prime + todo
  }

  private def mapRanges(values: List[AocRange], ranges: List[(AocRange, AocRange)]): List[AocRange] = {

    values.flatMap { value =>
      val rangesSorted = ranges.sortBy(r => r._1.start)
      help(value, rangesSorted).filter(r => r.length != 0)
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
