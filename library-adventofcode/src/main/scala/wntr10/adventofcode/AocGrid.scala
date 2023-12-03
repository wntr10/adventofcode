package wntr10.adventofcode

import scala.collection.immutable.SortedMap

final class AocGrid(val map: SortedMap[AocKey, AocValue]) {

  def filter(pred: AocNode => Boolean): AocGrid = {
    new AocGrid(map.filter(p => pred(AocNode(p._1, p._2))))
  }

  def foreach(f: AocNode => Unit): Unit = {
    map.foreach(p => f(AocNode(p._1, p._2)))
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
