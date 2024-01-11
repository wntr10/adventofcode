package wntr10.adventofcode

object AocNodes {

  def of(str: String, value: String): AocNode = {
    AocNode(AocQuad(str), AocStringValue(value))
  }

}
