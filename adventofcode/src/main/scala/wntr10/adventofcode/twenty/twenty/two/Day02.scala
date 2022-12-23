package wntr10.adventofcode.twenty.twenty.two

import wntr10.adventofcode.Day

private object Context {
  val ROCK = 1
  val PAPER = 2
  val SCISSORS = 3
  val beats: Map[Int, Int] = Map(ROCK -> SCISSORS, PAPER -> ROCK, SCISSORS -> PAPER)
  val beatenBy: Map[Int, Int] = beats.map(_.swap)
  val WIN = 6
  val TIE = 3
  val LOSE = 0
}

private case class Rps(value: Int) {

  lazy val beats: Rps = Rps(Context.beats(value))
  lazy val beatenBy: Rps = Rps(Context.beatenBy(value))

  def score(other: Rps): Int = {
    if (other == beats) {
      Context.WIN
    } else if (other == this) {
      Context.TIE
    } else {
      require(other == beatenBy)
      Context.LOSE
    }
  }

  def forScore(score: Int): Rps = {
    score match {
      case Context.LOSE => beats
      case Context.TIE => this
      case Context.WIN => beatenBy
    }
  }

  override def toString: String = {
    value match {
      case 1 => "Rock"
      case 2 => "Pape"
      case 3 => "Scis"
    }
  }
}


final class Day02(input: List[List[String]]) extends Day {

  private val rock = Rps(Context.ROCK)
  private val pape = Rps(Context.PAPER)
  private val scis = Rps(Context.SCISSORS)

  private val map = Map("A" -> rock, "B" -> pape, "C" -> scis, "X" -> rock, "Y" -> pape, "Z" -> scis)
  private val map2 = Map("A" -> rock.value, "B" -> pape.value, "C" -> scis.value, "X" -> 0, "Y" -> 3, "Z" -> 6)

  override def partOne(): String = {
    val p1 = input.map { row =>
      row.map(map)
    }

    val part1 = p1 map { round =>
      val you = round.last
      you.value + you.score(round.head)
    }

    part1.sum.toString
  }

  override def partTwo(): String = {
    val p2 = input.map { row =>
      row.map(map2)
    }

    val part2 = p2.map { round =>
      val you = Rps(round.head).forScore(round.last)
      you.value + round.last
    }

    part2.sum.toString
  }

}
