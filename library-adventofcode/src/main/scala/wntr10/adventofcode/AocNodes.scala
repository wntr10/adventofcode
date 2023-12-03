package wntr10.adventofcode

object AocNodes {

  def of(str: String, value: String): AocNode = {
    AocNode(AocKey(str), AocStringValue(value))
  }

}
