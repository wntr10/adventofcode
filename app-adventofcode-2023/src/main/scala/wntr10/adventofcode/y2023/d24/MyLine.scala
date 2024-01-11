package wntr10.adventofcode.y2023.d24

case class MyLine(direction: MyVector2D, originOffset: BigDecimal) {

  def abscissa(point: MyVector2D): BigDecimal = {
    direction.dot(point)
  }

  def intersection(other: MyLine): Option[MyVector2D] = {
    val area = this.direction.signedArea(other.direction)
    if (area == 0) {
      None
    } else {
      val x = MyVector2D.linearCombination(
        other.direction.x, originOffset,
        -direction.x, other.originOffset) / area
      val y = MyVector2D.linearCombination(
        other.direction.y, originOffset,
        -direction.y, other.originOffset) / area
      Some(MyVector2D(x, y))
    }
  }

  def offset(point: MyVector2D): BigDecimal = {
    originOffset - direction.signedArea(point)
  }
}


object MyLine {
  def fromPoints(p1: MyVector2D, p2: MyVector2D): MyLine = {
    fromPointAndDirection(p1, p1.vectorTo(p2))
  }

  def rayFromPointAndDirection(startPoint: MyVector2D, direction: MyVector2D): MyRay = {
    val line = fromPointAndDirection(startPoint, direction)
    MyRay(line, startPoint)
  }

  private def fromPointAndDirection(pt: MyVector2D, dir: MyVector2D): MyLine = {
    val normalizedDir = dir.normalize()
    val originOffset = normalizedDir.signedArea(pt)
    new MyLine(normalizedDir, originOffset)
  }
}
