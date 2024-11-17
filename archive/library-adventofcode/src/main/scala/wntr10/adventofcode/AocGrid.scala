package wntr10.adventofcode

import scala.collection.immutable.SortedMap

final class AocGrid(val map: SortedMap[AocKey, AocValue]) {

  private lazy val xy = {
    val set = map.toSet
    set.map { n =>
      val p = AocInterleave.squash(BigInt(n._1.str, 4))
      val node = AocNode(n._1, n._2)
      (p, node)
    }
  }

  private lazy val minX = xy.map(_._1._1).min
  private lazy val minY = xy.map(_._1._2).min
  private lazy val maxX = xy.map(_._1._1).max
  private lazy val maxY = xy.map(_._1._2).max

  lazy val topLeft: AocNode = xy.find(n => n._1._1 == minX && n._1._2 == maxY).map(_._2).get
  lazy val bottomRight: AocNode = xy.find(n => n._1._1 == maxX && n._1._2 == minY).map(_._2).get

  lazy val top: Set[AocNode] = xy.filter(_._1._2 == maxY).map(_._2)
  lazy val bottom: Set[AocNode] = xy.filter(_._1._2 == minY).map(_._2)

  lazy val left: Set[AocNode] = xy.filter(_._1._1 == minX).map(_._2)
  lazy val right: Set[AocNode] = xy.filter(_._1._1 == maxX).map(_._2)

  lazy val nodes: Set[AocNode] = {
    map.map(e => AocNode(e._1, e._2)).toSet
  }

  def merge(other: AocGrid): AocGrid = {
    val ita = map.iterator
    val itb = other.map.iterator

    var aopt = Option(ita.hasNext).filter(_ == true).map(_ => ita.next())
    var bopt = Option(itb.hasNext).filter(_ == true).map(_ => itb.next())
    implicit val ordering: AocKeyOrdering.type = AocKeyOrdering
    var prime = SortedMap.empty[AocKey, AocValue]
    while (true) {
      (aopt, bopt) match {
        case (None, None) =>
          return new AocGrid(prime)
        case (None, Some(n)) =>
          prime = prime.updated(n._1, n._2)
          bopt = Option(itb.hasNext).filter(_ == true).map(_ => itb.next())
        case (Some(n), None) =>
          prime = prime.updated(n._1, n._2)
          aopt = Option(ita.hasNext).filter(_ == true).map(_ => ita.next())
        case (Some(a), Some(b)) =>
          val c = ordering.compare(a._1, b._1)
          if (c < 0) {
            prime = prime.updated(a._1, a._2)
            aopt = Option(ita.hasNext).filter(_ == true).map(_ => ita.next())
          } else if (c > 0) {
            prime = prime.updated(b._1, b._2)
            bopt = Option(itb.hasNext).filter(_ == true).map(_ => itb.next())
          } else {
            prime = prime.updated(a._1, a._2)
            aopt = Option(ita.hasNext).filter(_ == true).map(_ => ita.next())
            bopt = Option(itb.hasNext).filter(_ == true).map(_ => itb.next())
          }
      }
    }
    this
  }

  def filter(pred: AocNode => Boolean): AocGrid = {
    new AocGrid(map.filter(p => pred(AocNode(p._1, p._2))))
  }

  def map(m: ((AocKey, AocValue)) => (AocKey, AocValue)): AocGrid = {
    implicit val ordering: AocKeyOrdering.type = AocKeyOrdering
    new AocGrid(map.map(p => m(p)))
  }

  def foreach(f: AocNode => Unit): Unit = {
    map.foreach(p => f(AocNode(p._1, p._2)))
  }

  def flood(start: AocNode, pred: AocValue => AocValue => Boolean): AocGrid = {
    var open = Set(start)
    val done = scala.collection.mutable.Set.empty[AocNode]
    val dirs = Set(1, 2, 3, 4)
    while (open.nonEmpty) {
      open = open.flatMap { n =>
        dirs.flatMap { d =>
          neighbor(n.key, d, pred(n.value))
        }.diff(done)
      }
      done.addAll(open)
    }
    implicit val ordering: AocKeyOrdering.type = AocKeyOrdering
    val sortedMap = SortedMap.from(done.map(n => (n.key, n.value)))
    new AocGrid(sortedMap)
  }

  def manhattan(from: AocNode, to: AocNode): BigInt = {
    val xyFrom = AocInterleave.squash(BigInt(from.key.str, 4))
    val xyTo = AocInterleave.squash(BigInt(to.key.str, 4))

    (xyTo._1 - xyFrom._1).abs + (xyTo._2 - xyFrom._2).abs
  }

  // TODO: Clean up
  def manhattan2(from: AocNode, to: AocNode, lenx: BigInt => BigInt, leny: BigInt => BigInt): BigInt = {
    val xyFrom = AocInterleave.squash(BigInt(from.key.str, 4))
    val xyTo = AocInterleave.squash(BigInt(to.key.str, 4))

    val xmin = xyTo._1.min(xyFrom._1)
    val xmax = xyTo._1.max(xyFrom._1)
    val ymin = xyTo._2.min(xyFrom._2)
    val ymax = xyTo._2.max(xyFrom._2)
    var c = BigInt(0)
    for (x <- xmin until xmax) {
      c = c + lenx(x)
    }

    for (y <- ymin until ymax) {
      c = c + leny(y)
    }

    c
  }

  def move(node: AocNode, dir: Int, pred: AocValue => Boolean, trash: AocValue): Option[AocGrid] = {
    val n = neighbor(node.key, dir, pred)
    if (n.isDefined) {
      val mapPrime = map.updated(n.get.key, node.value).updated(node.key, trash)
      Some(new AocGrid(mapPrime))
    } else {
      None
    }
  }

  def neighborModulo(key: AocKey, dir: Int, pred: AocValue => Boolean): Option[AocNode] = {
    AocInterleave.neighbor(key.str, dir).flatMap { n =>
      val k = AocQuad(n)
      val p = AocInterleave.squash(BigInt(k.str, 4))
      val pPrime = (p._1 % maxX, p._2 % maxY)

      val vOpt = map.get(k)
      vOpt.flatMap { v =>
        if (pred(v)) {
          Some(AocNode(k, v))
        } else {
          None
        }
      }
    }
  }

  def neighbor(key: AocKey, dir: Int, pred: AocValue => Boolean): Option[AocNode] = {
    AocInterleave.neighbor(key.str, dir).flatMap { n =>
      val k = AocQuad(n)
      val vOpt = map.get(k)
      vOpt.flatMap { v =>
        if (pred(v)) {
          Some(AocNode(k, v))
        } else {
          None
        }
      }
    }
  }

  def neighbors(key: AocKey, pred: AocValue => Boolean): List[AocNode] = {
    AocInterleave.neighbors(key.str).drop(1).filter(_.isDefined).map(_.get).flatMap { neighbor =>
      val k = AocQuad(neighbor)
      val vOpt = map.get(k)
      vOpt.flatMap { v =>
        if (pred(v)) {
          Some(AocNode(k, v))
        } else {
          None
        }
      }
    }
  }

  def show(pred: AocNode => Boolean): Unit = {
    val xy = map.map { kv =>
      val p = AocInterleave.squash(BigInt(kv._1.str, 4))
      val value = if (pred(AocNode(kv._1, kv._2))) {
        AocStringValue("@")
      } else {
        kv._2
      }
      (p._1.toInt, p._2.toInt) -> value
    }

    val maxX = xy.map(_._1._1).max
    val maxY = xy.map(_._1._2).max

    val arr = Array.tabulate(maxY + 1, maxX + 1)((y, x) => xy.get(x, maxY - y).getOrElse(AocStringValue(" ")))

    Range.inclusive(0, maxY).foreach { y =>
      Range.inclusive(0, maxX).foreach { x =>
        val str = arr(y)(x).str
        print(str)
      }
      println()
    }
  }

  def show(): Unit = {
    val xy = map.map { kv =>
      val p = AocInterleave.squash(BigInt(kv._1.str, 4))
      (p._1.toInt, p._2.toInt) -> kv._2
    }

    val maxX = xy.map(_._1._1).max
    val maxY = xy.map(_._1._2).max

    val arr = Array.tabulate(maxY + 1, maxX + 1)((y, x) => xy.get(x, maxY - y).getOrElse(AocStringValue(" ")))
    Range.inclusive(0, maxY).foreach { y =>
      Range.inclusive(0, maxX).foreach { x =>
        print(arr(y)(x).str)
      }
      println()
    }
  }

  def arr(background: String): Array[Array[String]] = {
    val xy = map.map { kv =>
      val p = AocInterleave.squash(BigInt(kv._1.str, 4))
      (p._1.toInt, p._2.toInt) -> kv._2
    }

    val maxX = xy.map(_._1._1).max
    val maxY = xy.map(_._1._2).max

    Array.tabulate[String](maxY + 1, maxX + 1)((y, x) => xy.get(x, maxY - y).getOrElse(AocStringValue(background)).str)
  }

}

object AocGrid {


  def repeat(grid: Array[Array[String]]): Array[Array[String]] = {
    val max = grid.map(_.length).max
    val max3 = grid.map(_.length).max * 3
    val gridPrime = Array.ofDim[String](grid.length * 3, max3)

    val len = grid.length
    var i = 0
    while (i < len) {
      val line = Array.ofDim[String](max3)
      val lineFilter = Array.ofDim[String](max3)
      val src = grid(i)
      val srcFilter = grid(i).map(s => if (s == "S") "." else s)

      System.arraycopy(srcFilter, 0, line, 0, max)
      System.arraycopy(src, 0, line, max, max)
      System.arraycopy(srcFilter, 0, line, max * 2, max)

      System.arraycopy(srcFilter, 0, lineFilter, 0, max)
      System.arraycopy(srcFilter, 0, lineFilter, max, max)
      System.arraycopy(srcFilter, 0, lineFilter, max * 2, max)

      gridPrime(i) = lineFilter
      gridPrime(i + len) = line
      gridPrime(i + len * 2) = lineFilter.clone()

      i += 1
    }
    gridPrime
  }

  def upscale(grid: Array[Array[String]], map: String => Array[Array[String]]): Array[Array[String]] = {
    val max = grid.map(_.length).max
    val max3 = grid.map(_.length).max * 3
    val gridPrime = Array.ofDim[String](grid.length * 3, max3)
    var i = 0
    while (i < grid.length) {
      val iScaled = i * 3
      val line0 = Array.ofDim[String](max3)
      val line1 = Array.ofDim[String](max3)
      val line2 = Array.ofDim[String](max3)

      var k = 0
      while (k < max) {
        val kScaled = k * 3
        val m = map(grid(i)(k))
        System.arraycopy(m(0), 0, line0, kScaled, 3)
        System.arraycopy(m(1), 0, line1, kScaled, 3)
        System.arraycopy(m(2), 0, line2, kScaled, 3)
        k += 1
      }

      gridPrime(iScaled) = line0
      gridPrime(iScaled + 1) = line1
      gridPrime(iScaled + 2) = line2

      i += 1
    }
    gridPrime
  }

  def pad(grid: Array[Array[String]], v: String): Array[Array[String]] = {
    val max = grid.map(_.length).max
    val gridPrime = Array.ofDim[String](grid.length + 2, max + 2)
    val line = Array.fill[String](max + 2)(v)
    var i = 0
    gridPrime(i) = line
    while (i < grid.length) {
      val line = Array.ofDim[String](max + 2)
      line(0) = v
      System.arraycopy(grid(i), 0, line, 1, max)
      line(max + 1) = v
      i += 1
      gridPrime(i) = line
    }
    i += 1
    gridPrime(i) = line
    gridPrime
  }

  def of(grid: Array[Array[String]], f: String => AocValue): AocGrid = {
    val maxY = grid.length - 1
    val maxX = grid.map(_.length).max - 1
    val max = AocInterleave.interleave(maxX, maxY).toString(4)
    val level = max.length
    implicit val ordering: AocKeyOrdering.type = AocKeyOrdering
    var map = SortedMap.empty[AocKey, AocValue]

    grid.zipWithIndex.foreach { case (line, y) =>
      line.zipWithIndex.foreach { case (value, x) =>
        val yPrime = maxY - y
        val key = AocInterleave.interleave(x, yPrime).toString(4)
        val keyPadded = if (key.length < level) {
          "0" * (level - key.length) + key
        } else {
          key
        }
        map = map.updated(AocQuad(keyPadded), f(value))
      }
    }

    new AocGrid(map)
  }

  def of(grid: Set[(Int, Int, String)], f: String => AocValue): AocGrid = {

    var gridPrime = grid

    var maxY = grid.map(_._2).max
    var maxX = grid.map(_._1).max

    val minY = grid.map(_._2).min
    val minX = grid.map(_._1).min

    //val width = maxX - minX
    //val height = maxY - minY

    if (minX < 0) {
      gridPrime = gridPrime.map { xy =>
        (xy._1 + Math.abs(minX), xy._2, xy._3)
      }
      maxX += Math.abs(minX)
    }

    if (minY < 0) {
      gridPrime = gridPrime.map { xy =>
        (xy._1, xy._2 + Math.abs(minY), xy._3)
      }
      maxY += Math.abs(minY)
    }

    val max = AocInterleave.interleave(maxX, maxY).toString(4)
    val level = max.length
    implicit val ordering: AocKeyOrdering.type = AocKeyOrdering
    var map = SortedMap.empty[AocKey, AocValue]

    gridPrime.foreach { xy =>
      val key = AocInterleave.interleave(xy._1, xy._2).toString(4)
      val keyPadded = if (key.length < level) {
        "0" * (level - key.length) + key
      } else {
        key
      }
      map = map.updated(AocQuad(keyPadded), f(xy._3))
    }

    new AocGrid(map)
  }

  def of2(grid: Set[(Int, Int, String)], f: String => AocValue): AocGrid = {

    val gridPrime = grid

    val maxY = grid.map(_._2).max
    val maxX = grid.map(_._1).max

    val max = AocInterleave.interleave(maxX, maxY).toString(4)
    val level = max.length
    implicit val ordering: AocKeyOrdering.type = AocKeyOrdering
    var map = SortedMap.empty[AocKey, AocValue]

    gridPrime.foreach { xy =>
      val key = AocInterleave.interleave(xy._1, xy._2).toString(4)
      val keyPadded = if (key.length < level) {
        "0" * (level - key.length) + key
      } else {
        key
      }
      map = map.updated(AocQuad(keyPadded), f(xy._3))
    }

    new AocGrid(map)
  }

}
