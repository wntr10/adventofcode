package wntr10.adventofcode

final case class AocNode(key: AocKey, value: AocValue) {
  def isNumber: Boolean = value.isNumber
}

object AocNode {
  def of(tuple: (AocKey, AocValue)): AocNode = {
    AocNode(tuple._1, tuple._2)
  }
}
