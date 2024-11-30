

trait Point {
  def x: BigInt

  def y: BigInt

  def of(x: BigInt, y: BigInt): Point
}

object Grid {

  private def pad(str: String, len: Int = 4): String = {
    val missing = (len - str.length).max(0)
    "_".repeat(missing) + str
  }

  def rows[P <: Point, T](map: Map[P, T], zero: T, visit: (BigInt, Map[BigInt, T]) => Unit): Unit = {
    if (map.isEmpty) {
      return
    }
    val factory = map.head._1

    val x = map.map(_._1.x)
    val y = map.map(_._1.y)
    val minX = x.min
    val minY = y.min
    val maxX = x.max
    val maxY = y.max

    var cy = minY
    while (cy < maxY + 1) {
      var row = Map.empty[BigInt, T]
      var cx = minX
      while (cx < maxX + 1) {
        val p = map.getOrElse(factory.of(cx, cy).asInstanceOf[P], zero)
        row = row.updated(cx, p)
        cx = cx + 1
      }
      visit(cy, row)
      cy = cy + 1
    }
  }

  def log[P <: Point, T](map: Map[P, T], zero: T, width: Int = 1): Unit = {
    if (map.isEmpty) {
      return
    }
    val factory = map.head._1

    val x = map.map(_._1.x)
    val y = map.map(_._1.y)
    val minX = x.min
    val minY = y.min
    val maxX = x.max
    val maxY = y.max

    var cy = minY
    while (cy < maxY + 1) {
      var cx = minX
      while (cx < maxX + 1) {
        val p = map.getOrElse(factory.of(cx, cy).asInstanceOf[P], zero)
        val str = p.toString
        print(pad(str, width))
        cx = cx + 1
      }
      println(s" $cy")
      cy = cy + 1
    }
  }
}
