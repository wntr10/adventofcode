package wntr10.adventofcode.y2023.d12

import wntr10.adventofcode.Input

import scala.annotation.tailrec


object PartOne extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.parse(lines)
  }

  lazy val length: Int = input.length

  @tailrec private def check(str: String, list: List[Int]): Boolean = {
    var prime = str
    while (prime.startsWith(".")) {
      prime = prime.substring(1)
    }
    if (list.isEmpty) {
      if (prime.isEmpty) {
        true
      } else {
        prime.forall(c => c == '.')
      }
    } else {
      val n = list.head
      val rest = list.drop(1)
      val oracle = "#".repeat(n)
      if (!prime.startsWith(oracle)) {
        false
      } else {
        prime = prime.substring(n)
        if (prime.isEmpty) {
          if (rest.isEmpty) {
            true
          } else {
            false
          }
        } else {
          if (!prime.startsWith(".")) {
            false
          } else {
            check(prime, rest)
          }
        }
      }
    }
  }

  private def replace(str: String, list: List[Boolean]): String = {
    var prime = str
    var lp = list
    while (lp.nonEmpty) {
      val h = lp.head
      lp = lp.drop(1)
      if (h) {
        prime = prime.replaceFirst("[?]", ".")
      } else {
        prime = prime.replaceFirst("[?]", "#")
      }
    }
    require(prime.forall(c => c != '?'))
    prime
  }

  private def solve(input: Parser.Alpha): BigInt = {
    input.map { l =>
      val q = l.a.count(c => c == '?')
      var configs = Set(List.empty[Boolean])

      Range(0, q).foreach { _ =>
        configs = configs.flatMap { o =>
          Set(true :: o, false :: o)
        }
      }

      var count = BigInt(0)
      configs.foreach { o =>
        val prime = replace(l.a, o)
        if (check(prime, l.b.toList)) {
          count += 1
        }
      }
      count
    }.sum
  }

  println(solve(input))

}
