package wntr10.adventofcode

final case class AocNode(key: AocKey, value: AocValue) {
  def isNumber: Boolean = value.isNumber
}
