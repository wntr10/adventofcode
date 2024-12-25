import $file.^.Basic, Basic._, Input._
import $file.^.Grid_v3, Grid_v3._

val ex = ".ex0" // 3
val inputParts = read(s"day25$ex")

val parts = split("\n\n", inputParts)

var locks = Set.empty[Vector[Int]]
var keys = Set.empty[Vector[Int]]

parts.foreach { input =>

  val lines = splitOn("\n")(input)
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

  def rotate(g: G[Char]): G[Char] = {
    g.invertY().swapXY()
  }

  val grid = G(prime, maxColumns, '.').trim()
  val gr = rotate(grid)
  val profile = gr.rows().map(_.count(_ == '#') - 1)
  if (grid.contains(P())) {
    locks += profile
  } else {
    keys += profile
  }
}

def fit(k: Vector[Int], l: Vector[Int]): Boolean = {
  k.zip(l).foreach { z =>
    if ((z._1 + z._2) > 5) return false
  }
  true
}

var fits = 0
keys.foreach { k =>
  locks.foreach { l =>
    if (fit(k, l)) {
      fits += 1
    }
  }
}

println(fits)
