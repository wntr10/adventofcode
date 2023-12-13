package wntr10.adventofcode.y2023.d13

import wntr10.adventofcode.Input


object PartTwo extends App {

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

  private def replace(str: String, idx: Int): String = {
    require(idx < str.length, str)

    val arr = str.toCharArray
    arr(idx) = if (arr(idx) == '#') {
      '.'
    } else {
      '#'
    }
    arr.mkString("")
  }

  private def findDelta(list: List[String], fx: Int, fy: Int) = {
    val old = findOld(list)
    val fix = fixAndFind(list, fx, fy)
    fix.diff(old)
  }

  private def findOld(list: List[String]) = {
    Range(0, list.head.length).map { c =>
      val isMirror = list.zipWithIndex.forall { lz =>
        isMirrorAt(lz._1, c)
      }
      (c, isMirror)
    }.filter(e => e._2).toList
  }

  private def fixAndFind(list: List[String], fx: Int, fy: Int) = {
    Range(0, list.head.length).map { c =>
      val isMirror = list.zipWithIndex.forall { lz =>
        val linePrime = if (lz._2 == fy) {
          replace(lz._1, fx)
        } else {
          lz._1
        }
        isMirrorAt(linePrime, c)
      }
      (c, isMirror)
    }.filter(e => e._2).toList
  }

  private def find(pattern: List[String], fx: Int, fy: Int): Int = {
    val reflection = findDelta(pattern, fx, fy)
    val patternRotated = rotate(pattern)
    val reflectionRotated = findDelta(patternRotated, fy, fx)

    val v = reflectionRotated.map(_._1 * 100).sum
    val h = reflection.map(_._1).sum

    h + v
  }

  private def findSmudge(list: List[String]): Int = {
    require(list.map(_.length).toSet.size == 1)

    Range(0, list.length).foreach { y =>
      Range(0, list.head.length).foreach { x =>
        val notes = find(list, x, y)
        if (notes != 0) {
          return notes
        }
      }
    }
    0
  }

  private def solve(input: Parser.Alpha): Int = {

    input.map { pattern =>
      findSmudge(pattern.toList)
    }.sum

  }

  println(solve(input))

}
