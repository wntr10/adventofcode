package wntr10.adventofcode.y2023.d24

final case class MyRay(line: MyLine, startPoint: MyVector2D) {
  def contains(pt: MyVector2D): Boolean = {
    val a1 = line.abscissa(startPoint)
    val a2 = line.abscissa(pt)
    a1.compare(a2) < 0
  }
}
