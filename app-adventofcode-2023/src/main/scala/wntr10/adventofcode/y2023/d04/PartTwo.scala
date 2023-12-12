package wntr10.adventofcode.y2023.d04

import com.google.common.base.Preconditions
import wntr10.adventofcode.Input

import java.util
import java.util.concurrent.TimeUnit


object PartTwo extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  private def solve(input: Parser.Alpha): Int = {

    Preconditions.checkArgument(input.zipWithIndex.forall(z => z._2 + 1 == z._1.a))
    val arr = Array.tabulate[Int](input.length)(_ => 1)

    val start = System.nanoTime()

    input.foreach { g =>
      val winning = g.b.a.sorted
      val youHave = g.b.b
      val idx = g.a - 1

      val matchingNumber = youHave.count(y => util.Arrays.binarySearch(winning, y) >= 0)
      
      val currentCopies = arr(idx)
      Range.inclusive(1, matchingNumber).foreach { m =>
        val sc = idx + m
        arr(sc) += currentCopies
      }
    }

    val solution = arr.sum

    println(TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start).toString + "ms")

    solution
  }

  println(solve(input))

}
