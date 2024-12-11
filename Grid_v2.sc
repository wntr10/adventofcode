import $ivy.`com.lihaoyi:fansi_2.13:0.5.0`
import $file.BigIntHelper_v1
import BigIntHelper_v1.BigIntHelper.vec

trait BigRange {
  def lower: BigInt

  def upper: BigInt

  def connected(other: BigRange): Boolean

  def intersection(other: BigRange): Option[BigRange]

  def split: Vector[BigRange]

  def isEmpty: Boolean = lower == upper

  def singleton: Boolean = lower + 1 == upper

  def size = upper - lower

  def contains(e: BigInt): Boolean

  def encloses(other: BigRange): Boolean

  def span(other: BigRange): BigRange

  def distance(e: BigInt): BigInt

}

trait BigPoint {
  def x: BigInt = 0

  def y: BigInt = 0

  def z: BigInt = 0

  def w: BigInt = 0

  def component(d: Int): BigInt

  def add(offset: BigInt*): BigPoint
}

trait BigGrid[T] {
  def zero: T

  def clear: BigGrid[T]

  def shape: Vector[BigInt]

  def ndim: Int = shape.length

  def size: BigInt = shape.product

  def delegate: Map[BigPoint, T]

  def ||(other: BigGrid[T]): BigGrid[T]

  def &&(other: BigGrid[T]): BigGrid[T]

  def fill(): BigGrid[T]

  def fill(e: T): BigGrid[T]

  def trim(): BigGrid[T]

  def apply(p: BigInt*): T

  def slice(p: BigRange*): BigGrid[T]

  def concatenate(other: BigGrid[T], axis: Int = 0): BigGrid[T]

  def contains(p: BigInt*): Boolean

  def intersection(p: BigInt*): Boolean

  def updated(p: BigInt*)(v: T): BigGrid[T]

  def log(): Unit

  def log(path: Vector[BigPoint]): Unit

  def find(e: T): Option[BigPoint]

  def findAll(e: T): Vector[BigPoint]

  def neighbors4(p: BigPoint): Set[BigPoint]

  def neighbors8(p: BigPoint): Set[BigPoint]

}


case class R(lower: BigInt, upper: BigInt) extends BigRange {
  require(lower <= upper, this)

  override def intersection(other: BigRange): Option[BigRange] = {
    if (lower >= other.upper || upper <= other.lower) return None
    val l = lower.max(other.lower)
    val u = upper.min(other.upper)
    if (l < u) {
      Some(R(l, u))
    } else {
      None
    }
  }

  override def split: Vector[BigRange] = {
    val midpoint = lower + ((upper - lower) >> 1)
    Vector(R(lower, midpoint), R(midpoint, upper))
  }

  override def toString = s"[$lower..$upper)"

  override def contains(e: BigInt): Boolean = e >= lower && e < upper

  override def encloses(inner: BigRange): Boolean = lower <= inner.lower && upper >= inner.upper

  override def span(other: BigRange): BigRange = R(lower.min(other.lower), upper.max(other.upper))

  override def connected(other: BigRange): Boolean = {
    lower < other.upper &&
      upper > other.lower &&
      lower.max(other.lower) < upper.min(other.upper)
  }

  override def distance(e: BigInt): BigInt = {
    if (contains(e)) {
      0
    } else if (e < lower) {
      (lower - e).abs
    } else {
      (e - upper).abs + 1
    }
  }
}

object R {

  def minMax(v: Vector[BigInt]): BigRange = {
    R(v.min, v.max + 1)
  }

}

case class P(override val x: BigInt = 0,
             override val y: BigInt = 0,
             override val z: BigInt = 0,
             override val w: BigInt = 0) extends BigPoint {

  override def component(d: Int): BigInt = d match {
    case 0 => x
    case 1 => y
    case 2 => z
    case 3 => w
  }

  override def add(offset: BigInt*): BigPoint = offset match {
    case Seq() => this
    case Seq(ox) => copy(x = x + ox)
    case Seq(oy, ox) => P(x = x + ox, y = y + oy)
    case Seq(oz, oy, ox) => P(x = x + ox, y = y + oy, z = z + oz)
    case Seq(ow, oz, oy, ox) => P(x = x + ox, y = y + oy, z = z + oz, w = w + ow)
  }
}

case class G[T](override val delegate: Map[BigPoint, T],
                override val shape: Vector[BigInt],
                override val zero: T) extends BigGrid[T] {

  private def of(p: BigInt*): BigPoint = {
    p match {
      case Seq(x) => P(x = x)
      case Seq(y, x) => P(x = x, y = y)
      case Seq(z, y, x) => P(x = x, y = y, z = z)
      case Seq(w, z, y, x) => P(x = x, y = y, z = z, w = w)
    }
  }

  override def toString = s"G(size=$size,shape=$shape,zero=<$zero>,#=${delegate.size})"

  override def log(): Unit = {
    println("--")
    rows(Vector.empty).zipWithIndex.foreach {
      case (r, i) =>
        println(s"$r |$i")
    }
  }

  override def log(path: Vector[BigPoint]): Unit = {
    println("--")
    rows(path).zipWithIndex.foreach {
      case (r, i) =>
        println(s"$r |$i")
    }
  }

  override def apply(p: BigInt*): T = {
    p match {
      case Seq(x) if shape.size == 1 =>
        delegate.getOrElse(of(x), zero)
      case Seq(x) if shape.size == 2 =>
        delegate.getOrElse(of(x / shape(1), x % shape(1)), zero)
      case Seq(y, x) if shape.size == 2 =>
        delegate.getOrElse(of(y, x), zero)
    }
  }


  def rows(path: Vector[BigPoint]): Vector[String] = {
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
        if (path.nonEmpty) {
          path.indexOf(of(cy, cx)) match {
            case -1 =>
              row = row + apply(cy, cx)
            case _ =>
              row = row + fansi.Str(apply(cy, cx).toString).overlay(fansi.Color.Red)
          }
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

  override def ||(other: BigGrid[T]): BigGrid[T] = {
    var prime = delegate
    other.delegate.foreach {
      case p -> v =>
        prime = prime.updated(p, v)
    }
    this.copy(delegate = prime, shape = shape.zip(other.shape).map(s => s._1.max(s._2)))
  }

  override def &&(other: BigGrid[T]): BigGrid[T] = {
    var prime = Map.empty[BigPoint, T]
    other.delegate.foreach {
      case p -> v if delegate.contains(p) =>
        prime = prime.updated(p, v)
    }
    this.copy(delegate = prime, shape = shape.zip(other.shape).map(s => s._1.min(s._2)))
  }

  override def fill(): BigGrid[T] = {
    fill(zero)
  }

  override def fill(e: T): BigGrid[T] = {
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

  override def updated(p: BigInt*)(v: T): BigGrid[T] = {
    val prime = p match {
      case Seq(x) if shape.size == 1 && x >= 0 && x < size =>
        delegate.updated(of(x), v)
      case Seq(x) if shape.size == 2 && x >= 0 && x < size =>
        delegate.updated(of(x / shape(1), x % shape(1)), v)
      case Seq(y, x) if shape.size == 2 && x >= 0 && x < shape(1) && y >= 0 && y <= shape(0) =>
        delegate.updated(of(y, x), v)
      case _ =>
        delegate
    }
    this.copy(delegate = prime)
  }

  override def trim(): BigGrid[T] = this.copy(delegate = delegate.filter(e => e._2 != zero))

  override def clear: BigGrid[T] = this.copy(delegate = Map.empty)

  override def contains(p: BigInt*): Boolean = apply(p: _*) != zero

  override def intersection(p: BigInt*): Boolean = {
    p.zip(shape).forall {
      case (c, b) if c >= 0 && c < b => true
      case _ => false
    }
  }

  override def concatenate(other: BigGrid[T], axis: Int = 0): BigGrid[T] = {
    val shapePrime = shape.zip(other.shape).zipWithIndex.map {
      case ((a, b), i) if i == axis =>
        a + b
      case ((a, b), _) if a == b => a
    }


    if (axis == 0 || axis == 1) {
      val offsets = other.shape.map(_ => BigInt(0)).updated(axis, shape(axis))
      println(other.delegate)
      println(offsets)
      val prime = other.delegate.map(e => (e._1.add(offsets: _*), e._2))
      println(prime)
      this.copy(delegate = delegate ++ prime, shape = shapePrime)
    } else {
      other
    }
  }

  private def pointIn(p: BigPoint, rs: Seq[BigRange]): Boolean = {
    rs.reverse.zipWithIndex.forall(r => r._1.contains(p.component(r._2)))
  }

  override def slice(p: BigRange*): BigGrid[T] = {
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

  override def find(e: T): Option[BigPoint] = {
    delegate.find(me => me._2 == e).map(_._1)
  }

  override def findAll(e: T): Vector[BigPoint] = {
    delegate.filter(me => me._2 == e).keys.toVector
  }

  override def neighbors8(p: BigPoint): Set[BigPoint] = {
    var ns = Set.empty[BigPoint]
    vec(-1, 0, 1).foreach { dx =>
      vec(-1, 0, 1).foreach { dy =>
        val n = P(p.x + dx, p.y + dy)
        if (n != p && intersection(n.y, n.x)) {
          ns = ns + n
        }
      }
    }
    require(ns.size <= 8)
    ns
  }

  override def neighbors4(p: BigPoint): Set[BigPoint] = {
    var ns = Set.empty[BigPoint]
    vec(-1, 0, 1).foreach { dx =>
      vec(-1, 0, 1).foreach { dy =>
        if (dx.abs != dy.abs) {
          val n = P(p.x + dx, p.y + dy)
          if (intersection(n.y, n.x)) {
            ns = ns + n
          }
        }
      }
    }
    require(ns.size <= 4)
    ns
  }
}

object G {

  def empty[T](shape: Vector[BigInt], zero: T): BigGrid[T] = {
    new G(Map.empty[BigPoint, T], shape, zero)
  }

  def ones(shape: Vector[BigInt]): BigGrid[BigInt] = {
    empty[BigInt](shape, BigInt(0)).fill(BigInt(1))
  }

  def apply[T](lines: Vector[Vector[T]], maxColumns: Int, zero: T): BigGrid[T] = {
    var map = Map.empty[BigPoint, T]
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
