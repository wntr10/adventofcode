package wntr10.adventofcode.y2023.d19

import wntr10.adventofcode.Input

object PartTwo extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.parse(lines)
  }

  private val empty: Map[String, (Int, Int)] = Map("x" -> (1, 1), "m" -> (1, 1), "a" -> (1, 1), "s" -> (1, 1))

  private case class Sorter(ranges: Map[String, (Int, Int)] = Map("x" -> (1, 4001), "m" -> (1, 4001), "a" -> (1, 4001), "s" -> (1, 4001))) {
    def range(category: String): (Int, Int) = ranges(category)

    def combinations: BigInt = ranges.values.map(r => BigInt(r._2 - r._1)).product
  }

  private case class Workflow(rules: List[String]) {

    def receive(sorter: Sorter, ws: Map[String, Workflow]): List[Sorter] = {
      receiveInt(sorter, rules, ws)
    }

    private def splitLess(sorter: Sorter, category: String, value: Int) = {
      val r = sorter.range(category)
      val rA = (r._1, value)
      val rB = (value, r._2)
      val rangesA = sorter.ranges.updated(category, rA)
      val rangesB = sorter.ranges.updated(category, rB)
      List(Sorter(rangesA), Sorter(rangesB))
    }

    private def splitGreater(sorter: Sorter, category: String, value: Int) = {
      val r = sorter.range(category)
      val rA = (value + 1, r._2)
      val rB = (r._1, value + 1)
      val rangesA = sorter.ranges.updated(category, rA)
      val rangesB = sorter.ranges.updated(category, rB)
      List(Sorter(rangesA), Sorter(rangesB))
    }

    private def receiveInt(sorter: Sorter, rules: List[String], ws: Map[String, Workflow]): List[Sorter] = {
      require(rules.nonEmpty)

      val rule = rules.head

      rule match {
        case s"$p<$v:A" =>
          val split = splitLess(sorter, p, v.toInt)
          split.head :: receiveInt(split.last, rules.drop(1), ws)
        case s"$p>$v:A" =>
          val split = splitGreater(sorter, p, v.toInt)
          split.head :: receiveInt(split.last, rules.drop(1), ws)
        case s"$p<$v:R" =>
          val split = splitLess(sorter, p, v.toInt)
          receiveInt(split.last, rules.drop(1), ws)
        case s"$p>$v:R" =>
          val split = splitGreater(sorter, p, v.toInt)
          receiveInt(split.last, rules.drop(1), ws)
        case s"$p>$v:$w" =>
          val split = splitGreater(sorter, p, v.toInt)
          List(ws(w).receive(split.head, ws), receiveInt(split.last, rules.drop(1), ws)).flatten
        case s"$p<$v:$w" =>
          val split = splitLess(sorter, p, v.toInt)
          List(ws(w).receive(split.head, ws), receiveInt(split.last, rules.drop(1), ws)).flatten
        case "A" => List(sorter)
        case "R" => List(Sorter(empty))
        case w => ws(w).receive(sorter, ws)
      }
    }
  }

  private def solve(input: Parser.Alpha): BigInt = {

    val workflows = input.a.map { w =>
      w.a -> Workflow(w.b.toList)
    }.toMap

    workflows("in").receive(Sorter(), workflows).map(_.combinations).sum
  }

  println(solve(input))

}
