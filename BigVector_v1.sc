// adapted from org.apache.commons.math3

final case class BigVector(x: BigDecimal, y: BigDecimal) {
  def vectorTo(v: BigVector): BigVector = {
    v.subtract(this)
  }

  def subtract(v: BigVector): BigVector = {
    BigVector(x - v.x, y - v.y)
  }

  def add(factor: BigDecimal, v: BigVector): BigVector = {
    BigVector(x + (factor * v.x), y + (factor * v.y))
  }

  def dot(v: BigVector): BigDecimal = {
    BigVector.linearCombination(x, v.x, y, v.y)
  }

  def norm(): BigDecimal = {
    var n = new java.math.BigDecimal(0)
    n = n.add(x.pow(2).bigDecimal)
    n = n.add(y.pow(2).bigDecimal)
    n.sqrt(BigDecimal.defaultMathContext)
  }

  def normalize(): BigVector = {
    val normInv = BigDecimal(1.0) / norm()
    BigVector(x * normInv, y * normInv)
  }

  def signedArea(v: BigVector): BigDecimal = {
    BigVector.linearCombination(x, v.y, -y, v.x)
  }
}

object BigVector {

  def of(x: BigDecimal, y: BigDecimal): BigVector = BigVector(x, y)

  def linearCombination(a1: BigDecimal, b1: BigDecimal, a2: BigDecimal, b2: BigDecimal): BigDecimal = {
    var sum = BigDecimal(0)
    sum += a1 * b1
    sum += a2 * b2
    sum
  }
}

final case class BigRay(line: BigLine, startPoint: BigVector) {
  def contains(pt: BigVector): Boolean = {
    val a1 = line.abscissa(startPoint)
    val a2 = line.abscissa(pt)
    a1.compare(a2) < 0
  }
}

final case class BigLine(direction: BigVector, originOffset: BigDecimal) {

  def abscissa(point: BigVector): BigDecimal = {
    direction.dot(point)
  }

  def contains(point: BigVector): Boolean = {
    //println(offset(point))
    offset(point).abs < 0.00000001
  }

  def intersection(other: BigLine): Option[BigVector] = {
    val area = this.direction.signedArea(other.direction)
    if (area == 0) {
      None
    } else {
      val x = BigVector.linearCombination(
        other.direction.x, originOffset,
        -direction.x, other.originOffset) / area
      val y = BigVector.linearCombination(
        other.direction.y, originOffset,
        -direction.y, other.originOffset) / area
      Some(BigVector(x, y))
    }
  }

  def offset(point: BigVector): BigDecimal = {
    originOffset - direction.signedArea(point)
  }
}

object BigLine {
  def fromPoints(p1: BigVector, p2: BigVector): BigLine = {
    fromPointAndDirection(p1, p1.vectorTo(p2))
  }

  def rayFromPointAndDirection(startPoint: BigVector, direction: BigVector): BigRay = {
    val line = fromPointAndDirection(startPoint, direction)
    BigRay(line, startPoint)
  }

  def fromPointAndDirection(pt: BigVector, dir: BigVector): BigLine = {
    val normalizedDir = dir.normalize()
    val originOffset = normalizedDir.signedArea(pt)
    new BigLine(normalizedDir, originOffset)
  }
}
