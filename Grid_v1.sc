
trait Point {
  def x: BigInt = 0

  def y: BigInt = 0

  def z: BigInt = 0

  def w: BigInt = 0
}

trait Grid[T] {
  def zero: T

  def shape: Vector[BigInt]

  def ndim: Int = shape.length

  def size: BigInt = shape.product

  def delegate: Map[Point, T]

  def apply(x: BigInt, y: BigInt): T

  def apply(p: Vector[BigInt]): T

  def of(x: BigInt, y: BigInt): Point

  def log(): Unit

  def rows: Vector[String]

  def columns: Vector[String]

  def diagonal1: Vector[String]

  def diagonal2: Vector[String]
}

case class P(override val x: BigInt,
             override val y: BigInt) extends Point {

}

final class G[T](override val delegate: Map[Point, T],
                 override val shape: Vector[BigInt],
                 override val zero: T) extends Grid[T] {

  override def of(x: BigInt, y: BigInt): Point = {
    P(x, y)
  }

  override def toString = delegate.toString()

  override def log(): Unit = {
    rows.zipWithIndex.foreach {
      case (r, i) =>
        println(s"$r $i")
    }
  }

  override def apply(p: Vector[BigInt]): T = delegate.getOrElse(of(p.head, p.last), zero)

  override def apply(x: BigInt, y: BigInt): T = delegate.getOrElse(of(x, y), zero)

  private def scan(idx: Vector[Int]): Vector[String] = {
    var result = Vector.empty[String]

    if (delegate.isEmpty) {
      return result
    }

    val minX = BigInt(0)
    val minY = BigInt(0)
    val upperX = shape(idx.head)
    val upperY = shape(idx.last)

    var cy = minY
    while (cy < upperY) {
      var cx = minX
      var row = ""
      while (cx < upperX) {
        var p = Vector(BigInt(0), BigInt(0))
        p = p.updated(idx.head, cx)
        p = p.updated(idx.last, cy)
        row = row + apply(p)
        cx = cx + 1
      }
      result = result :+ row
      cy = cy + 1
    }
    result
  }

  override def rows: Vector[String] = {
    scan(Vector(0, 1))
  }

  override def columns: Vector[String] = {
    scan(Vector(1, 0))
  }

  override def diagonal1: Vector[String] = {
    var result = Vector.empty[String]

    if (delegate.isEmpty) {
      return result
    }

    val minX = BigInt(0)
    val minY = BigInt(0)
    val upperX = shape(0)
    val upperY = shape(1)

    var sy = minY
    while (sy < upperY) {
      var cx = minX
      var cy = sy
      var row = ""
      while (cx < upperX && cy >= minY) {
        row = row + apply(cx, cy)
        cx = cx + 1
        cy = cy - 1
      }
      result = result :+ row
      sy = sy + 1
    }

    var sx = 1
    while (sx < upperX) {
      var cx = sx
      var cy = upperY - 1
      var row = ""
      while (cx < upperX && cy >= minY) {
        row = row + apply(cx, cy)
        cx = cx + 1
        cy = cy - 1
      }
      result = result :+ row
      sx = sx + 1
    }

    result
  }

  override def diagonal2: Vector[String] = {
    var result = Vector.empty[String]

    if (delegate.isEmpty) {
      return result
    }

    val minX = BigInt(0)
    val minY = BigInt(0)
    val upperX = shape(0)
    val upperY = shape(1)

    var sy = minY
    while (sy < upperY) {
      var cx = upperX - 1
      var cy = sy
      var row = ""
      while (cx >= minX && cy >= minY) {
        row = row + apply(cx, cy)
        cx = cx - 1
        cy = cy - 1
      }
      result = result :+ row
      sy = sy + 1
    }

    var sx = upperX - 2
    while (sx >= minX) {
      var cx = sx
      var cy = upperY - 1
      var row = ""
      while (cx >= minX && cy >= minY) {
        row = row + apply(cx, cy)
        cx = cx - 1
        cy = cy - 1
      }
      result = result :+ row
      sx = sx - 1
    }

    result
  }
}

object G {
  def apply[T](lines: Vector[Vector[T]], maxColumns: Int, zero: T): Grid[T] = {
    var map = Map.empty[Point, T]
    lines.zipWithIndex.foreach {
      case (l, y) =>
        l.zipWithIndex.foreach {
          case (c, x) =>
            map = map.updated(P(x, y), c)
        }
    }
    new G(map, Vector(maxColumns, lines.length), zero)
  }
}
