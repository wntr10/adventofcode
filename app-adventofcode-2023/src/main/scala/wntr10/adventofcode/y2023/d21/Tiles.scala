package wntr10.adventofcode.y2023.d21

import wntr10.adventofcode.{AocKey, AocPoint}

class Tiles(val map: Map[(BigInt, BigInt), Set[AocKey]]) {

  def diff(st: Tiles): Tiles = {
    var reMap = map
    st.map.foreach { t =>
      val opt = map.get(t._1)
      opt.foreach { st =>
        val pr = st.diff(t._2)
        if (pr.nonEmpty) {
          reMap = reMap.updated(t._1, pr)
        } else {
          reMap = reMap.removed(t._1)
        }
      }
    }
    new Tiles(reMap)
  }

  def size: Int = {
    map.flatMap(e => e._2).size
  }

  def tiles: (BigInt, BigInt, BigInt, BigInt) = {
    val xs = map.keySet.map(_._1)
    val ys = map.keySet.map(_._2)
    (xs.min, xs.max, ys.min, ys.max)
  }

  def move(x: BigInt, y: BigInt): Tiles = {
    var reMap = Map.empty[(BigInt, BigInt), Set[AocKey]]
    map.foreach { w =>
      val k = (w._1._1 + x, w._1._2 + y)
      reMap = reMap.updated(k, w._2)
    }
    new Tiles(reMap)
  }

  def group: Map[(BigInt, BigInt), Set[AocKey]] = {
    map
  }

  def update(x: BigInt, y: BigInt, v: Set[AocKey]): Tiles = {
    if (v.nonEmpty) {
      new Tiles(map.updated((x, y), v))
    } else {
      this
    }
  }

  def merge(st: Tiles): Tiles = {
    var reMap = map
    st.map.foreach { t =>
      if (t._2.nonEmpty) {
        val r = reMap.getOrElse(t._1, Tiles.EMPTY)
        reMap = reMap.updated(t._1, t._2 ++ r)
      }
    }
    new Tiles(reMap)
  }
}

object Tiles {

  case class Wrapper(virt: ((BigInt, BigInt), (BigInt, BigInt)))

  val EMPTY = Set.empty[AocKey]

  private val EMPTY_MAP = Map.empty[(BigInt, BigInt), Set[AocKey]]

  val empty: Tiles = {
    new Tiles(EMPTY_MAP)
  }

  def from(ws: Set[Wrapper]): Tiles = {
    var current = EMPTY_MAP
    ws.foreach { r =>
      val s = current.getOrElse(r.virt._1, EMPTY)
      current = current.updated(r.virt._1, s + AocPoint(r.virt._2._1, r.virt._2._2))
    }
    new Tiles(current)
  }

  def from(x: BigInt, y: BigInt, tu: (Set[AocKey], Set[AocKey], Set[AocKey], Set[AocKey], Set[AocKey])): Tiles = {
    var current = EMPTY_MAP
    val c = tu._1
    if (c.nonEmpty) {
      current = current.updated((x, y), c)
    }

    val l = tu._2
    if (l.nonEmpty) {
      current = current.updated((x - 1, y), l)
    }

    val r = tu._3
    if (r.nonEmpty) {
      current = current.updated((x + 1, y), r)
    }

    val t = tu._4
    if (t.nonEmpty) {
      current = current.updated((x, y + 1), t)
    }

    val b = tu._5
    if (b.nonEmpty) {
      current = current.updated((x, y - 1), b)
    }
    new Tiles(current)
  }

}
