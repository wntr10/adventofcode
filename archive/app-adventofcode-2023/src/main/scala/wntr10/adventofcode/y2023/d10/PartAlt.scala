package wntr10.adventofcode.y2023.d10

import wntr10.adventofcode._

object PartAlt extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  lazy val length: Int = input.length


  private def solve(input: Parser.Alpha): Int = {
    val map = Map(
      "." -> Array(
        Array(".", ".", "."),
        Array(".", ".", "."),
        Array(".", ".", ".")
      ),
      "|" -> Array(
        Array(".", "X", "."),
        Array(".", "X", "."),
        Array(".", "X", ".")
      ),
      "-" -> Array(
        Array(".", ".", "."),
        Array("X", "X", "X"),
        Array(".", ".", ".")
      ),
      "F" -> Array(
        Array(".", ".", "."),
        Array(".", "X", "X"),
        Array(".", "X", ".")
      ),
      "J" -> Array(
        Array(".", "X", "."),
        Array("X", "X", "."),
        Array(".", ".", ".")
      ),
      "L" -> Array(
        Array(".", "X", "."),
        Array(".", "X", "X"),
        Array(".", ".", ".")
      ),
      "7" -> Array(
        Array(".", ".", "."),
        Array("X", "X", "."),
        Array(".", "X", ".")
      ),
      "S" -> Array(
        Array(".", "S", "."),
        Array("S", "S", "S"),
        Array(".", "S", ".")
      )
    )

    val inputScaled = AocGrid.upscale(input, map)
    val inputPadded = AocGrid.pad(inputScaled, ".")
    val grid = AocGrid.of(inputPadded, s => AocStringValue(s))
    val loop = grid.flood(grid.nodes.filter(n => n.value.str == "S").head, _ => v => v.str != ".")
    val subGrid = grid.map(kv => (kv._1, AocStringValue(".")))

    loop.merge(subGrid).show()

    -1
  }

  println(solve(input))

}
