package wntr10.adventofcode.y2023.d13

import wntr10.adventofcode.Input


object PartOne extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.parse(lines)
  }

  lazy val length: Int = input.length

  private def isMirrorAt(str: String, split: Int): Boolean = {
    val left = str.substring(0, split)
    val right = str.substring(split, str.length)
    val min = Math.min(left.length, right.length)
    if (min >= 1) {
      val leftPrime = left.reverse.substring(0, min)
      val rightPrime = right.substring(0, min)
      leftPrime == rightPrime
    } else {
      false
    }
  }

  private def rotate(list: List[String]): List[String] = {
    Range(0, list.head.length).map { c =>
      list.map(r => r.charAt(c)).mkString("")
    }.toList
  }

  private def find(list: List[String]) = {
    Range(0, list.head.length).map { c =>
      val isMirror = list.zipWithIndex.forall { lz =>
        isMirrorAt(lz._1, c)
      }
      (c, isMirror)
    }.filter(e => e._2).toList
  }

  private def solve(input: Parser.Alpha): Int = {

    input.map { arr =>
      val pattern = arr.toList

      val reflection = find(pattern)
      val patternRotated = rotate(pattern)
      val reflectionRotated = find(patternRotated)

      val v = reflectionRotated.map(_._1 * 100).sum
      val h = reflection.map(_._1).sum

      h + v
    }.sum

  }

  println(solve(input))

}
