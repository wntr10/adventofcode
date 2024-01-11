package wntr10.adventofcode

final case class AocPoint(x: BigInt, y: BigInt) extends AocKey {
  override def str: String = ???

  override def xy: (BigInt, BigInt) = (x, y)
}
