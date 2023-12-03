package wntr10.adventofcode

case class AocStringValue(str: String) extends AocValue {

  def isNumber: Boolean = str.forall(_.isDigit)

}
