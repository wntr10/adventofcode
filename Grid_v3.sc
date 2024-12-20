import $ivy.`com.lihaoyi:fansi_2.13:0.5.0`
import com.google.common.collect.Iterables
import com.google.common.io.Files

import java.io.File
import java.nio.charset.Charset
import scala.jdk.CollectionConverters.{IterableHasAsJava, IteratorHasAsScala}


final case class R(lower: BigInt, upper: BigInt) {
  require(lower <= upper, this)

  def intersection(other: R): Option[R] = {
    if (lower >= other.upper || upper <= other.lower) return None
    val l = lower.max(other.lower)
    val u = upper.min(other.upper)
    if (l < u) {
      Some(R(l, u))
    } else {
      None
    }
  }

  def split: Vector[R] = {
    val midpoint = lower + ((upper - lower) >> 1)
    Vector(R(lower, midpoint), R(midpoint, upper))
  }

  override def toString = s"[$lower..$upper)"

  def singleton: Boolean = lower + 1 == upper

  def contains(e: BigInt): Boolean = e >= lower && e < upper

  def encloses(inner: R): Boolean = lower <= inner.lower && upper >= inner.upper

  def span(other: R): R = R(lower.min(other.lower), upper.max(other.upper))

  def connected(other: R): Boolean = {
    lower < other.upper &&
      upper > other.lower &&
      lower.max(other.lower) < upper.min(other.upper)
  }

  def distance(e: BigInt): BigInt = {
    if (contains(e)) {
      0
    } else if (e < lower) {
      (lower - e).abs
    } else {
      (e - upper).abs + 1
    }
  }

  def size = upper - lower
}

object R {

  def minMax(v: Vector[BigInt]): R = {
    R(v.min, v.max + 1)
  }

}

final case class P(x: BigInt = 0,
                   y: BigInt = 0,
                   z: BigInt = 0,
                   w: BigInt = 0) {

  def component(d: Int): BigInt = d match {
    case 0 => x
    case 1 => y
    case 2 => z
    case 3 => w
  }

  def add(offset: BigInt*): P = offset match {
    case Seq() => this
    case Seq(ox) => copy(x = x + ox)
    case Seq(oy, ox) => P(x = x + ox, y = y + oy)
    case Seq(oz, oy, ox) => P(x = x + ox, y = y + oy, z = z + oz)
    case Seq(ow, oz, oy, ox) => P(x = x + ox, y = y + oy, z = z + oz, w = w + ow)
  }

  def manhattan(other: P): BigInt = {
    (other.w - w).abs + (other.z - z).abs + (other.y - y).abs + (other.x - x).abs
  }

}

final case class G[E](delegate: Map[P, E],
                      shape: Vector[BigInt],
                      zero: E) {

  // forwards

  def size: Int = delegate.size

  def foreach(f: ((P, E)) => Unit): Unit = delegate.foreach(f)

  def foreachPoint(f: P => Unit): Unit = delegate.foreach(l => f(l._1))

  def findElement(e: E): Option[P] = delegate.find(_._2 == e).map(_._1)

  def contains(p: P): Boolean = delegate.contains(p)

  def get(p: P): Option[E] = delegate.get(p)

  def getOrElseZero(p: P): E = delegate.getOrElse(p, zero)

  def mapElement(f: E => E): G[E] = {
    val prime = delegate.map { e =>
      e._1 -> f(e._2)
    }
    copy(delegate = prime)
  }

  def removedAll(other: G[E]): G[E] = {
    this.copy(delegate = delegate.removedAll(other.delegate.keys))
  }

  // alias

  def not(other: G[E]) = removedAll(other)

  //

  def cropped(): G[E] = {
    copy(delegate = delegate.filter(isInBounds))
  }

  def isVector: Boolean = {
    if (shape.size != 2) return false
    val set = shape.toSet
    set.size == 2 && set.contains(1)
  }

  private def of(p: BigInt*): P = {
    p match {
      case Seq(x) => P(x = x)
      case Seq(y, x) => P(x = x, y = y)
      case Seq(z, y, x) => P(x = x, y = y, z = z)
      case Seq(w, z, y, x) => P(x = x, y = y, z = z, w = w)
    }
  }

  private def adapt(p: Seq[BigInt]): Seq[BigInt] = {
    p match {
      case Seq(x) if shape.size == 1 && x >= 0 =>
        p
      case Seq(x) if shape.size == 1 =>
        Seq(shape(0) + x)
      case Seq(x) if shape.size == 2 && x >= 0 =>
        Seq(x / shape(1), x % shape(1))
      case Seq(x) if shape.size == 2 =>
        val prime = size + x
        Seq(prime / shape(1), prime % shape(1))
      case Seq(y, x) if shape.size == 2 =>
        val yPrime = if (y >= 0) y else shape(0) + y
        val xPrime = if (x >= 0) x else shape(1) + x
        Seq(yPrime, xPrime)
    }
  }

  override def toString = s"G(size=$size,shape=$shape,zero=<$zero>,#=${delegate.size})"

  def log(): Unit = {
    println("--")
    if (shape.size == 1) {
      Range(0, shape(0).toInt).foreach { i =>
        print(apply(i))
      }
      println()
    } else {
      rows(Vector.empty).zipWithIndex.foreach {
        case (r, i) =>
          println(s"$r |$i")
      }
    }
  }

  def write(f: File): Unit = {
    Files.asCharSink(f, Charset.defaultCharset()).write(rows().mkString("\n"))
  }

  def log(path: Vector[P]): Unit = {
    println("--")
    rows(Seq(Set.empty, path.toSet)).zipWithIndex.foreach {
      case (r, i) =>
        println(s"$r |$i")
    }
  }

  def logWithColors(sets: Set[P]*): Unit = {
    println("--")
    rows(sets).zipWithIndex.foreach {
      case (r, i) =>
        println(s"$r |$i")
    }
  }

  def apply(p: BigInt*): E = {
    delegate.getOrElse(of(adapt(p): _*), zero)
  }

  private val colorList = List(fansi.Color.Blue, fansi.Color.Red, fansi.Color.Green, fansi.Color.Yellow, fansi.Color.Magenta)

  private val it = Iterables.cycle(colorList.asJava).iterator().asScala

  def rows(colors: Seq[Set[P]] = Seq.empty): Vector[String] = {

    val idx = colors.zip(it)
    var result = Vector.empty[String]

    val minY = BigInt(0)
    val minX = BigInt(0)
    val upperY = shape(0)
    val upperX = shape(1)

    var cy = minY
    while (cy < upperY) {
      var cx = minX
      var row = ""
      while (cx < upperX) {
        if (idx.nonEmpty) {
          val point = of(cy, cx)
          var str = fansi.Str(apply(cy, cx).toString)
          idx.foreach { c =>
            if (c._1.contains(point)) {
              str = str.overlay(c._2)
            }
          }
          row = row + str
        } else {
          row = row + apply(cy, cx)
        }
        cx = cx + 1
      }
      result = result :+ row
      cy = cy + 1
    }
    result
  }

  def invertX(): G[E] = {
    require(shape.size == 2)
    if (shape(1) == 0) return this
    val maxX = shape(1) - 1
    val prime = delegate.map { e =>
      (e._1.copy(x = maxX - e._1.x), e._2)
    }
    copy(delegate = prime)
  }


  def invertY(): G[E] = {
    require(shape.size == 2)
    if (shape(0) == 0) return this
    val maxY = shape(0) - 1
    val prime = delegate.map { e =>
      (e._1.copy(y = maxY - e._1.y), e._2)
    }
    copy(delegate = prime)
  }

  private def swap(v: Vector[BigInt], a: Int, b: Int): Vector[BigInt] = {
    v.updated(a, v(b)).updated(b, v(a))
  }

  def swapXY(): G[E] = {
    require(shape.size == 2)
    val prime = delegate.map { e =>
      (e._1.copy(x = e._1.y, y = e._1.x), e._2)
    }
    copy(delegate = prime, shape = swap(shape, 0, 1))
  }

  private def rotate(): G[E] = {
    require(shape.size == 2)
    val prime = delegate.map { e =>
      (e._1.copy(x = e._1.y, y = e._1.x), e._2)
    }
    copy(delegate = prime)
  }

  def or(grids: (G[E], G[E])): G[E] = {
    val (a, b) = if (grids._1.size >= grids._2.size) {
      grids
    } else {
      grids.swap
    }

    def remapping(e: E)(current: Option[E]): Option[E] = {
      if (current.isEmpty) {
        Some(e)
      } else {
        current
      }
    }

    var prime = a.delegate
    b.delegate.foreach {
      case p -> v =>
        prime = prime.updatedWith(p)(remapping(v))
    }
    a.copy(delegate = prime, shape = a.shape.zip(b.shape).map(s => s._1.max(s._2)))
  }


  def or(other: G[E]): G[E] = or((this, other))

  private def and(grids: (G[E], G[E])): G[E] = {
    val (a, b, ab) = if (grids._1.size <= grids._2.size) {
      (grids._1, grids._2, true)
    } else {
      (grids._2, grids._1, false)
    }

    var prime = Map.empty[P, E]
    a.delegate.foreach {
      case p -> v =>
        b.delegate.get(p).foreach { eb =>
          prime = prime.updated(p, if (ab) v else eb)
        }
      case _ => // skip
    }
    this.copy(delegate = prime, shape = a.shape.zip(b.shape).map(s => s._1.min(s._2)))
  }

  def and(other: G[E]): G[E] = and((this, other))

  def fill(): G[E] = {
    fill(zero)
  }

  def fill(e: E): G[E] = {
    var prime = delegate
    val minX = BigInt(0)
    val minY = BigInt(0)
    val upperY = shape(0)
    val upperX = shape(1)

    var cy = minY
    while (cy < upperY) {
      var cx = minX
      while (cx < upperX) {
        val p = of(cy, cx)
        if (!prime.contains(p)) {
          prime = prime.updated(p, e)
        }
        cx = cx + 1
      }
      cy = cy + 1
    }
    this.copy(delegate = prime)
  }

  def updated(p: BigInt*)(v: E): G[E] = {
    this.copy(delegate = delegate.updated(of(adapt(p): _*), v))
  }

  def trim(): G[E] = this.copy(delegate = delegate.filter(e => e._2 != zero))

  def clear: G[E] = this.copy(delegate = Map.empty)

  def isInBounds(l: (P, E)): Boolean = isInBounds(l._1)

  def isInBounds(p: P): Boolean = isInBounds(p.y, p.x)

  def isInBounds(p: BigInt*): Boolean = {
    p.zip(shape).forall {
      case (c, b) if c >= 0 && c < b => true
      case _ => false
    }
  }

  def concatenate(other: G[E], axis: Int = 0): G[E] = {
    val shapePrime = shape.zip(other.shape).zipWithIndex.map {
      case ((a, b), i) if i == axis =>
        a + b
      case ((a, b), _) if a == b => a
    }

    if (axis == 0 || axis == 1) {
      val offsets = other.shape.map(_ => BigInt(0)).updated(axis, shape(axis))
      val prime = other.delegate.map(e => (e._1.add(offsets: _*), e._2))
      this.copy(delegate = delegate ++ prime, shape = shapePrime)
    } else {
      other
    }
  }

  private def pointIn(p: P, rs: Seq[R]): Boolean = {
    rs.reverse.zipWithIndex.forall(r => r._1.contains(p.component(r._2)))
  }

  def slice(p: R*): G[E] = {
    val pLimited = p.zipWithIndex.map {
      case (r, i) =>
        R(0, shape(i)).intersection(r).getOrElse(R(0, 0))
    }

    val shapePrime = shape.zipWithIndex.map {
      case (_, i) if i < p.size => pLimited(i).size
      case (upper, _) => upper
    }

    val offsets = p.map(r => -r.lower)
    val prime = delegate.view
      .filter(e => pointIn(e._1, p))
      .map(e => (e._1.add(offsets: _*), e._2))
      .toMap

    this.copy(delegate = prime, shape = shapePrime)
  }

  def findAll(e: E): Vector[P] = {
    delegate.filter(me => me._2 == e).keys.toVector
  }
}

object G {

  def empty[T](shape: Vector[BigInt], zero: T): G[T] = {
    new G(Map.empty[P, T], shape, zero)
  }

  def ones(shape: Vector[BigInt]): G[BigInt] = {
    empty[BigInt](shape, BigInt(0)).fill(BigInt(1))
  }

  def apply[T](lines: Vector[Vector[T]], maxColumns: Int, zero: T): G[T] = {
    var map = Map.empty[P, T]
    lines.zipWithIndex.foreach {
      case (l, y) =>
        l.zipWithIndex.foreach {
          case (c, x) =>
            map = map.updated(P(x, y), c)
        }
    }
    new G(map, Vector(lines.length, maxColumns), zero)
  }

}
