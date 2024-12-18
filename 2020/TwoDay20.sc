import $file.^.Basic
import Basic._
import Input._
import $file.^.Grid_v3
import Grid_v3._

val ex = ".ex0" // 273
val inputRaw = read(s"day20.mod$ex")
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

val grid = G(prime, maxColumns, '.').trim()


def invSwap(): Set[G[Char]] = {
  var set = Set(grid)
  var stop = false
  while (!stop) {
    var prime = Set.empty[G[Char]]
    set.foreach { s =>
      prime += s.swapXY()
      prime += s.invertY()
    }
    if (set == prime) {
      stop = true
    } else {
      set ++= prime
    }
  }
  set
}

val set = invSwap()

val monster = Vector(
  "..................#.".toVector,
  "#....##....##....###".toVector,
  ".#..#..#..#..#..#...".toVector)

val mg = G(monster, 20, '.').trim()

set.foreach { s =>
  var c = Set.empty[P]
  for (y <- 0 until (s.shape(0).toInt - 3)) {
    for (x <- 0 until (s.shape(1).toInt - 20)) {
      val window = s.slice(R(y, y + 3), R(x, x + 20)).trim()
      if ((mg && window) == mg) {
        c ++= mg.delegate.keySet.map(k => k.add(y, x))
      }
    }
  }
  if (c.nonEmpty) {
    s.logWithColors(c)

    val roughness = (s.delegate.keySet -- c).size
    println(roughness)
  }
}
