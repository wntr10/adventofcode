package wntr10.adventofcode.y2023.d06


object PartTwo extends App {

  private def solve(): Long = {

    val races = Map(
      40929790 -> 215106415051100L
    )

    races.map { race =>
      var beat = 0L
      Range.inclusive(0, race._1).foreach { hold =>
        val timeLeft = race._1 - hold
        if (timeLeft.toLong * hold > race._2) {
          beat += 1
        }
      }
      beat
    }.product

  }

  println(solve())

}
