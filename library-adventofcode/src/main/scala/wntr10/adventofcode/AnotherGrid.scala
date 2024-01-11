package wntr10.adventofcode

final class AnotherGrid(map: Map[AocPoint, AocValue]) {
  lazy val nodes: Set[AocNode] = {
    map.map(e => AocNode(e._1, e._2)).toSet
  }

  private lazy val minX = nodes.map(_.key.xy._1).min
  private lazy val minY = nodes.map(_.key.xy._2).min
  private lazy val maxX = nodes.map(_.key.xy._1).max
  private lazy val maxY = nodes.map(_.key.xy._2).max

  def manhattan(from: AocNode, to: AocNode): BigInt = {
    val xyFrom = from.key.xy
    val xyTo = to.key.xy

    (xyTo._1 - xyFrom._1).abs + (xyTo._2 - xyFrom._2).abs
  }

  def virtual()(key: AocKey)  = {
    val xy = key.xy
    sub(xy._1, xy._2)
  }

  private def sub(x: BigInt, y: BigInt) = {
    var xd = x.abs / (maxX + 1)
    var yd = y.abs / (maxY + 1)

    var xm = x.abs % (maxX + 1)
    var ym = y.abs % (maxY + 1)

    if (x < 0) {
      if (xm != 0) {
        xm = (maxX - xm) + 1
      }
      xd = -(xd + 1)
    }

    if (y < 0) {
      if (ym != 0) {
        ym = (maxY - ym) + 1
      }
      yd = -(yd + 1)
    }

    ((xd, yd), (xm, ym))
  }

  private def virtual(x: BigInt, y: BigInt) = {
    var xm = x.abs % (maxX + 1)
    var ym = y.abs % (maxY + 1)

    if (xm != x) {
      //("from x " + x + " to " + xm)
    }

    if (ym != y) {
      // println("from y " + y + " to " + ym)
    }


    if (x < 0) {
      //println(" on top from xm " + xm)
      if (xm == 0) {
        xm
      } else {
        xm = (maxX - xm) + 1
      }

      // println("  to xm " + xm)
    }

    if (y < 0) {
      if (ym == 0) {
        ym
      } else {
        ym = (maxY - ym) + 1
      }
      //println(" on top from ym " + ym)

      //println("  to ym " + ym)
    }

    val km = AocPoint(xm, ym)
    map.get(km)
  }

  def neighbor(key: AocKey, dir: Int, pred: AocValue => Boolean): Option[AocNode] = {
    val xy = key.xy
    var x = xy._1
    var y = xy._2

    dir match {
      case 1 =>
        x += 1
      case 2 =>
        x -= 1
      case 3 =>
        y -= 1
      case 4 =>
        y += 1
    }

    val vOpt = virtual(x, y)

    val k = AocPoint(x, y)
    vOpt.flatMap { v =>
      if (pred(v)) {
        Some(AocNode(k, v))
      } else {
        None
      }
    }
  }

  def show(): Unit = {
    val _maxX = maxX
    val _maxY = maxY
    val _minX = minX
    val _minY = minY

    for (y <- _maxY.to(_minY, -1)) {
      for (x <- _minX to _maxX) {
        print(virtual(x, y).getOrElse(AocStringValue("X")).str)
      }
      println()
    }
  }

  def show(span: Map[AocKey, AocValue]): Unit = {
    val xy = span.keySet.map(_.xy)
    val _maxX = xy.map(_._1).max.max(maxX)
    val _maxY = xy.map(_._2).max.max(maxY)
    val _minX = xy.map(_._1).min.min(minX)
    val _minY = xy.map(_._2).min.min(minY)

    val offsetX = if (_minX < 0) {
      _minX.abs
    } else {
      BigInt(0)
    }

    val offsetY = if (_minY < 0) {
      _minY.abs
    } else {
      BigInt(0)
    }

    for (y <- _maxY.to(_minY, -1)) {
      for (x <- _minX to _maxX) {
        val p = AocPoint(x, y)
        if (span.contains(p)) {
          print(span(p).str)
        } else {
          print(virtual(x, y).getOrElse(AocStringValue("X")).str)
        }
      }
      println()
    }
  }

  def show2(span: Map[AocKey, AocValue]): Unit = {
    val xy = span.keySet.map(_.xy)
    val _maxX = xy.map(_._1).max.max(maxX)
    val _maxY = xy.map(_._2).max.max(maxY)
    val _minX = xy.map(_._1).min.min(minX)
    val _minY = xy.map(_._2).min.min(minY)

    for (y <- _maxY.to(_minY, -1)) {
      for (x <- _minX to _maxX) {
        print(virtual(x, y).getOrElse(AocStringValue("X")).str)
      }
      println()
    }
  }

}

object AnotherGrid {
  def of(grid: Array[Array[String]], f: String => AocValue): AnotherGrid = {
    val maxY = grid.length - 1

    var map = Map.empty[AocPoint, AocValue]

    grid.zipWithIndex.foreach { case (line, y) =>
      line.zipWithIndex.foreach { case (value, x) =>
        val yPrime = maxY - y
        map = map.updated(AocPoint(x, yPrime), f(value))
      }
    }

    new AnotherGrid(map)
  }


}