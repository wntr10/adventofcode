import $file.^.Basic
import Basic._
import Input._
import $file.^.Grid_v3
import Grid_v3._
import $file.^.BigIntHelper_v1
import BigIntHelper_v1.BigIntHelper.vec

import scala.annotation.tailrec

val ex = ".ex0" // 18
val inputRaw = read(s"day04$ex")
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
println(s"#lines: ${prime.size}")
println(s"#columns: $maxColumns")

val grid = G(prime, maxColumns, '.').trim()

def run(): Seq[Set[P]] = {
  var r = Seq.empty[Set[P]]

  @tailrec
  def search(p: P, dx: BigInt, dy: BigInt, str: Vector[Char], trace: Set[P] = Set.empty): Set[P] = {
    if (!grid.isInBounds(p) || str.isEmpty) return Set.empty

    val c = grid.get(p)
    str match {
      case h +: _ if h != c =>
        Set.empty
      case Vector(_) =>
        trace + p
      case _ =>
        search(p.add(dy, dx), dx, dy, str.drop(1), trace + p)
    }
  }

  grid.findAll('X').foreach { p =>
    vec(-1, 0, 1).foreach { dy =>
      vec(-1, 0, 1).foreach { dx =>
        if (Set(dx, dy) != Set(0)) {
          r = r :+ search(p, dx, dy, "XMAS".toVector)
        }
      }
    }
  }

  r
}

val rs = run().filter(_.nonEmpty)

grid.logWithColors(rs: _*)

println(rs.size)
