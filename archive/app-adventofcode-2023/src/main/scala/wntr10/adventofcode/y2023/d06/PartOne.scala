package wntr10.adventofcode.y2023.d06

object PartOne extends App {

  private def solve(): Int = {
    val races = Map(
      40 -> 215,
      92 -> 1064,
      97 -> 1505,
      90 -> 1100
    )

    races.map { race =>
      var beat = 0
      Range.inclusive(0, race._1).foreach { hold =>
        val timeLeft = race._1 - hold
        if (timeLeft * hold > race._2) {
          beat += 1
        }
      }
      beat
    }.product
  }

  println(solve())

}
