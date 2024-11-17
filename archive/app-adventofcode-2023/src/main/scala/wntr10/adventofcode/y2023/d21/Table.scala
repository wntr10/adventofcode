package wntr10.adventofcode.y2023.d21

import wntr10.adventofcode.Data

object Table {

  var table = Map.empty[(Char, Int, Int, Int, Int, Int), Int]
  var tiles = Map.empty[Int, Int]

  def read(): Unit = {
    val data = new Data("table.map")
    data.read.split('\n').foreach {
      case s"('$a', $c, $l, $r, $t, $b) -> $v," =>
        table = table.updated((a.head, c.toInt, l.toInt, r.toInt, t.toInt, b.toInt), v.toInt)
    }
  }

  def readTiles(): Unit = {
    val data = new Data("tiles.map")
    data.read.split('\n').foreach {
      case s"$s -> $t," =>
        tiles = tiles.updated(s.toInt, t.toInt)
    }
  }

}
