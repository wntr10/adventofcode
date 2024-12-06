
trait Range {
  def lower: BigInt

  def upper: BigInt

  def connected(other: Range): Boolean

  def intersection(other: Range): Option[Range]

  def split: (Range, Range)

  def isEmpty: Boolean = lower == upper

  def singleton: Boolean = lower + 1 == upper

  def size = upper - lower

  def contains(e: BigInt): Boolean

  def encloses(other: Range): Boolean

  def span(other: Range): Range

}

case class R(lower: BigInt, upper: BigInt) extends Range {
  require(lower <= upper, this)

  override def intersection(other: Range): Option[Range] = {
    if (lower >= other.upper || upper <= other.lower) return None
    val l = lower.max(other.lower)
    val u = upper.min(other.upper)
    if (l < u) {
      Some(R(l, u))
    } else {
      None
    }
  }

  override def split: (Range, Range) = {
    val midpoint = lower + ((upper - lower) >> 1)
    (R(lower, midpoint), R(midpoint, upper))
  }

  override def toString = s"[$lower..$upper)"

  override def contains(e: BigInt): Boolean = e >= lower && e < upper

  override def encloses(inner: Range): Boolean = lower <= inner.lower && upper >= inner.upper

  override def span(other: Range): Range = R(lower.min(other.lower), upper.max(other.upper))

  override def connected(other: Range): Boolean = {
    lower < other.upper &&
      upper > other.lower &&
      lower.max(other.lower) < upper.min(other.upper)
  }
}

object R {

  def minMax(v: Vector[BigInt]): Range = {
    R(v.min, v.max + 1)
  }

}