package wntr10.adventofcode.y2023.d18

final case class Block(x: BigInt, y: BigInt, width: BigInt, height: BigInt) {
  require(width >= 0)
  require(height >= 0)

  val maxX: BigInt = x + width - 1

  val maxY: BigInt = y + height - 1
}
