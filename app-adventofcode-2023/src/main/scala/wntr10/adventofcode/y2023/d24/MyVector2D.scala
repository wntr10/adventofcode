package wntr10.adventofcode.y2023.d24

final case class MyVector2D(x: BigDecimal, y: BigDecimal) {
  def vectorTo(v: MyVector2D): MyVector2D = {
    v.subtract(this)
  }

  def subtract(v: MyVector2D): MyVector2D = {
    MyVector2D(x - v.x, y - v.y)
  }

  def add(factor: BigDecimal, v: MyVector2D): MyVector2D = {
    MyVector2D(x + (factor * v.x), y + (factor * v.y))
  }

  def dot(v: MyVector2D): BigDecimal = {
    MyVector2D.linearCombination(x, v.x, y, v.y)
  }

  def norm(): BigDecimal = {
    var n = new java.math.BigDecimal(0)
    n = n.add(x.pow(2).bigDecimal)
    n = n.add(y.pow(2).bigDecimal)
    n.sqrt(BigDecimal.defaultMathContext)
  }

  def normalize(): MyVector2D = {
    val normInv = BigDecimal(1.0) / norm()
    MyVector2D(x * normInv, y * normInv)
  }

  def signedArea(v: MyVector2D): BigDecimal = {
    MyVector2D.linearCombination(
      x, v.y,
      -y, v.x)
  }
}

object MyVector2D {

  def of(x: BigDecimal, y: BigDecimal): MyVector2D = MyVector2D(x, y)

  def linearCombination(a1: BigDecimal, b1: BigDecimal, a2: BigDecimal, b2: BigDecimal): BigDecimal = {
    var sum = BigDecimal(0)
    sum += a1 * b1
    sum += a2 * b2
    sum
  }
}