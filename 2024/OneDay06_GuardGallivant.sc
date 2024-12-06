import $file.^.Basic, Basic._, Input._
import $file.^.Grid_v1, Grid_v1._

val ex = ".ex0" // 41
val inputRaw = read(s"day06$ex")
val lines = splitOn("\n")(inputRaw)

type VALUE = Char
var map = Map.empty[P, VALUE]
var countRest = 0

var start = P(0, 0)

def visit(line: String, idx: BigInt): Unit = {
  (line, idx) match {
    case (s"$columns", y) =>
      columns.zipWithIndex.foreach {
        case ('^', x) =>
          start = P(x, y)
          map = map.updated(P(x, y), '.')
        case (c, x) =>
          map = map.updated(P(x, y), c)
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

var visit = Set.empty[P]

def run(): Boolean = {
  var direction = 0 // up
  var pos = start
  while (true) {
    visit = visit + pos

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
    }
  }
  false
}

run()
println(visit.size)
