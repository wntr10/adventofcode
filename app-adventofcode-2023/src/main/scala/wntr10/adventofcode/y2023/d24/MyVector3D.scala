package wntr10.adventofcode.y2023.d24

final case class MyVector3D(x: BigDecimal, y: BigDecimal, z: BigDecimal) {
  def vectorTo(v: MyVector3D): MyVector3D = {
    v.subtract(this)
  }

  def subtract(v: MyVector3D): MyVector3D = {
    MyVector3D(x - v.x, y - v.y, z - v.z)
  }

  def add(factor: BigDecimal, v: MyVector3D): MyVector3D = {
    MyVector3D(x + (factor * v.x), y + (factor * v.y), z + (factor * v.z))
  }

  def dot(v: MyVector3D): BigDecimal = {
    MyVector3D.linearCombination(x, v.x, y, v.y, z, v.z)
  }

  def cross(v: MyVector3D): MyVector3D = {
    MyVector3D(MyVector3D.linearCombination(y, v.z, -z, v.y),
      MyVector3D.linearCombination(z, v.x, -x, v.z),
      MyVector3D.linearCombination(x, v.y, -y, v.x))
  }

  def normalize(): MyVector3D = {
    val normInv = BigDecimal(1) / norm()
    MyVector3D(x * normInv, y * normInv, z * normInv)
  }

  def norm(): java.math.BigDecimal = {
    var sum = new java.math.BigDecimal(0)

    sum = sum.add(x.pow(2).bigDecimal)
    sum = sum.add(y.pow(2).bigDecimal)
    sum = sum.add(z.pow(2).bigDecimal)

    sum.sqrt(BigDecimal.defaultMathContext)
  }
}

object MyVector3D {

  def of(x: BigDecimal, y: BigDecimal, z: BigDecimal): MyVector3D = MyVector3D(x, y, z)

  def linearCombination(a1: BigDecimal, b1: BigDecimal, a2: BigDecimal, b2: BigDecimal): BigDecimal = {
    var sum = BigDecimal(0)
    sum += a1 * b1
    sum += a2 * b2
    sum
  }

  def linearCombination(a1: BigDecimal, b1: BigDecimal, a2: BigDecimal, b2: BigDecimal, a3: BigDecimal, b3: BigDecimal): BigDecimal = {
    var sum = BigDecimal(0)
    sum += a1 * b1
    sum += a2 * b2
    sum += a3 * b3
    sum
  }
}
