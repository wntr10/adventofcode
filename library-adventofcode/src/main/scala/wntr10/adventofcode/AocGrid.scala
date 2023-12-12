package wntr10.adventofcode

import scala.collection.immutable.SortedMap

final class AocGrid(val map: SortedMap[AocKey, AocValue]) {

  def nodes(): Set[AocNode] = {
    map.map(e => AocNode(e._1, e._2)).toSet
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

  def neighbor(key: AocKey, dir: Int, pred: AocValue => Boolean): Option[AocNode] = {
    AocInterleave.neighbor(key.str, dir).flatMap { n =>
      val k = AocKey(n)
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
      val k = AocKey(neighbor)
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
      println
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
      println
    }
  }
}

object AocGrid {

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
        map = map.updated(AocKey(keyPadded), f(value))
      }
    }

    new AocGrid(map)
  }
}
