import $file.^.Basic, Basic._, Input._
import $file.^.Grid_v1, Grid_v1._

val ex = ".ex0" // 6
val inputRaw = read(s"day06$ex")
val lines = splitOn("\n")(inputRaw)

type VALUE = Char
var original = Map.empty[P, VALUE]
var countRest = 0

var start = P(0, 0)

def visit(line: String, idx: BigInt): Unit = {
  (line, idx) match {
    case (s"$columns", y) =>
      columns.zipWithIndex.foreach {
        case ('^', x) =>
          start = P(x, y)
          original = original.updated(P(x, y), '.')
        case (c, x) =>
          original = original.updated(P(x, y), c)
        case _ =>
        // skip
      }
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

println(start)

def run(map: Map[P, VALUE]): Boolean = {
  var direction = 0 // up
  var pos = start
  var history = Set.empty[(Int, P)]
  while (true) {
    history = history + ((direction, pos))
    val lookahead = direction match {
      case 0 =>
        pos.copy(y = pos.y - 1)
      case 1 =>
        pos.copy(x = pos.x + 1)
      case 2 =>
        pos.copy(y = pos.y + 1)
      case 3 =>
        pos.copy(x = pos.x - 1)
    }
    val n = map.getOrElse(lookahead, '$')
    n match {
      case '$' =>
        return false
      case '.' =>
        pos = lookahead
      case _ =>
        direction = (direction + 1) % 4
        if (history.contains((direction, pos))) {
          return true
        }
    }
  }
  false
}

var count = 0
original.foreach { m =>
  if (m._1 != start && m._2 == '.') {
    val prime = original.updated(m._1, '#')
    if (run(prime)) {
      count = count + 1
    }
  }
}

println(count)
