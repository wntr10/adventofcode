import $file.^.Basic
import Basic._
import Input._
import $file.^.Bag_v1
import Bag_v1._
import $file.^.Grid_v3
import Grid_v3._
import $file.^.BigIntHelper_v1
import BigIntHelper_v1._
import BigIntHelper.vec

import scala.annotation.tailrec

val ex = ".ex0"
val inputRaw = read(s"day20$ex")
val lines = splitOn("\n")(inputRaw)

type E = Char
type LINE = Vector[E]

var prime = Vector.empty[LINE]
var countRest = 0
var maxColumns = 0

def visit(line: String, idx: BigInt): Unit = {
  (line, idx) match {
    case (s"$str", _) =>
      val l = str.toVector
      prime = prime :+ l
      maxColumns = Math.max(maxColumns, l.length)
    case (l, i) =>
      countRest = countRest + 1
      println(s"REST ${pad(i)}: <$l>")
  }
}

lines.zipWithIndex.foreach {
  case (lines, idx) =>
    visit(lines, idx)
}

require(countRest == 0)

var grid = G(prime, maxColumns, '#').trim()

val end = grid.findElement('E').get
val start = grid.findElement('S').get

grid = grid.updated(end.y, end.x)('.')
grid = grid.updated(start.y, start.x)('.')

@tailrec
def bfs(filter: P => Boolean)(nxt: Vector[P],
                              done: Map[P, BigInt],
                              bound: Option[BigInt] = None): Map[P, BigInt] = {

  if (nxt.isEmpty) return done
  val p = nxt.head
  var nxtPrime = nxt.drop(1)
  val step = done(p)

  var donePrime = done
  if (bound.isEmpty || step + 1 < bound.get) {
    vec(-1, 0, 1).foreach { dy =>
      vec(-1, 0, 1).foreach { dx =>
        if (dy.abs != dx.abs) {
          val n = p.add(dy, dx)
          if (!donePrime.contains(n) && filter(n)) {
            nxtPrime = nxtPrime :+ n
            donePrime = donePrime.updated(n, step + 1)
          }
        }
      }
    }
  }

  bfs(filter)(nxtPrime, donePrime, bound)
}

val flooded = bfs(grid.contains)(Vector(start), Map(start -> 0))

Set(3, 21).foreach { cheat =>
  var savings = List.empty[BigInt]
  grid.foreachPoint { p =>
    val circle = bfs(grid.isInBounds)(Vector(p), Map(p -> 0), Some(cheat)).filter(c => c._2 != 0 && grid.contains(c._1))
    circle.foreach { pc =>
      val d = ((flooded(pc._1) - flooded(p)) - p.manhattan(pc._1)).max(0)
      if (d > 0) {
        savings = d :: savings
      }
    }
  }

  println(Bag.of(savings))
  println(Bag.of(savings.filter(_ >= 50)))
  println(Bag.of(savings).filter(_ >= 100).sum())

}
