import $ivy.`org.jgrapht:jgrapht-core:1.5.2`
import $ivy.`com.google.guava:guava:33.3.1-jre`
import $file.^.Basic
import Basic._
import Input._
import $file.^.Grid_v2
import Grid_v2._
import $file.^.BigIntHelper_v1
import BigIntHelper_v1.BigIntHelper.vec

val ex = ".ex0" // 36, 81
val inputRaw = read(s"day10$ex")
val lines = splitOn("\n")(inputRaw)

type LINE = Vector[Char]

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

var grid = G(prime, maxColumns, '.')

grid.log()

def neighbors(x: BigInt, y: BigInt): Set[BigPoint] = {
  var nxt = Set.empty[BigPoint]
  val level = grid(y, x)
  vec(-1, 0, 1).foreach { dy =>
    vec(-1, 0, 1).foreach { dx =>
      if (dy.abs != dx.abs) {
        val nx = x + dx
        val ny = y + dy
        val levelNeighbor = grid(ny, nx)

        levelNeighbor match {
          case '.' =>
          case nl if (nl.toInt - 1) == level.toInt =>
            nxt = nxt + P(nx, ny)
          case _ =>
        }
      }
    }
  }
  nxt
}

println(grid)

def hasPath(s: BigPoint, e: BigPoint): Boolean = {
  if (s == e) {
    return true
  }
  neighbors(s.x, s.y).foreach { n =>
    if (hasPath(n, e)) return true
  }
  false
}

def search(s: BigPoint): Int = {
  if (grid(s.y, s.x) == '9') {
    return 1
  }
  var score = 0
  neighbors(s.x, s.y).foreach { n =>
    score = score + search(n)
  }
  score
}

var sumScores = BigInt(0)
var sumRating = BigInt(0)

grid.findAll('0').foreach { a =>

  var score = BigInt(0)
  grid.findAll('9').foreach { b =>
    if (hasPath(a, b)) {
      score = score + 1
    }
  }
  sumScores = sumScores + score
  sumRating = sumRating + search(a)
}

println(s"Part One: $sumScores")
println(s"Part Two: $sumRating")
