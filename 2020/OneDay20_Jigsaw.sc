import $file.^.Basic
import Basic._
import Input._
import $file.^.Grid_v3
import Grid_v3._
import $file.^.BigIntHelper_v1
import BigIntHelper_v1.BigIntHelper.vec

import java.io.File

type VALUE = Char

case class TILE(id: Int, variant: Int, map: G[Char]) {
  override def toString: String = s"TILE($id, $variant)"
}

var tiles = Map.empty[Int, Set[TILE]]

val ex = ".ex0" // 20899048083289

val input = read(s"day20$ex")
val parts = split("Tile ", input)

def invSwap(grid: G[Char]): Set[G[Char]] = {
  var set = Set(grid)
  var stop = false
  while (!stop) {
    var prime = Set.empty[G[Char]]
    set.foreach { s =>
      prime += s.invertY()
      prime += s.swapXY()
    }
    if (set == prime) {
      stop = true
    } else {
      set ++= prime
    }
  }
  set
}


parts.foreach { partRaw =>
  var g = G.empty(vec(10, 10), '.')
  val lines = splitOn("\n")(partRaw)
  val id = lines.head.dropRight(1).toInt
  lines.drop(1).zipWithIndex.foreach {
    case (str, y) =>
      str.zipWithIndex.foreach {
        case (c, x) =>
          g = g.updated(y, x)(c)
      }
  }

  val m = invSwap(g)
  tiles = tiles.updated(id, m.zipWithIndex.map(v => TILE(id, v._2, v._1)))
}

val tk = tiles.keys.toIndexedSeq.sorted

tk.foreach { k =>
  println(s"$k => ${tiles(k).size}")
  tiles(k).head.map.log()
}

def permuteSets[T](visit: Vector[T] => Unit, prune: Vector[T] => Boolean)
                  (v: Vector[Set[T]], chosen: Vector[T]): Unit = {

  if (v.isEmpty) {
    visit(chosen)
  } else {
    v.indices.foreach { i =>
      val (left, right) = v.splitAt(i)
      right.head.foreach { c =>
        val pick = chosen :+ c
        if (!prune(pick)) {
          permuteSets(visit, prune)(left ++ right.drop(1), pick)
        }
      }
    }
  }
}

def run(): Unit = {
  val square = new LinearSquare(tk.size)
  var first = true

  def visit(v: Vector[TILE]): Unit = {
    println("==")
    println(square.corners.map(c => BigInt(v(c.toInt).id)).product)
    val prime = v.map(_.map.slice(R(1, 9), R(1, 9)))

    var idx = 0
    var r = Option.empty[G[Char]]
    Range(0, square.side.toInt).foreach { _ =>
      var c = Option.empty[G[Char]]
      Range(0, square.side.toInt).foreach { _ =>
        if (c.isEmpty) {
          c = Some(prime(idx))
        } else {
          c = Some(c.get.concatenate(prime(idx), axis = 1))
        }
        idx += 1
      }
      if (r.isEmpty) {
        r = c
      } else {
        r = Some(r.get.concatenate(c.get))
      }
    }

    if (first) {
      r.get.write(new File(s"day20.mod$ex"))
      first = false
    }
  }

  def prune(v: Vector[TILE]): Boolean = {
    var nok = false

    def x(a: BigInt, b: BigInt): Unit = {
      val lu = lineUpX(v(a.toInt).map, v(b.toInt).map)
      nok = nok || !lu
    }

    def y(a: BigInt, b: BigInt): Unit = {
      val lu = lineUpY(v(a.toInt).map, v(b.toInt).map)
      nok = nok || !lu
    }

    square.vistLast(v, x, y)
    nok
  }

  val v = tiles.values.toVector

  permuteSets[TILE](visit, prune)(v, Vector.empty)

}

def lineUpX(a: G[Char], b: G[Char]): Boolean = {
  a.slice(R(0, 10), R(9, 10)) == b.slice(R(0, 10), R(0, 1))
}

def lineUpY(a: G[Char], b: G[Char]): Boolean = {
  a.slice(R(9, 10), R(0, 10)) == b.slice(R(0, 1), R(0, 10))
}

run()
