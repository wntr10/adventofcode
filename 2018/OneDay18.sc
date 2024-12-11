import $file.^.Basic
import Basic._
import $file.^.Grid_v2, Grid_v2._
import Basic.Input.{read, splitOn}


val ex = ".ex0" // 1147
val inputRaw = read(s"day18$ex")
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

var grid = G(prime, maxColumns, '.').trim()

def resourceValue(grid: BigGrid[Char]): Int = {
  grid.findAll('|').size * grid.findAll('#').size
}

def ctx(grid: BigGrid[Char])(ns: Set[BigPoint]): (Int, Int, Int) = {
  // .toList since we need to count
  val mm = ns.toList.map(p => grid(p.y, p.x))
  val o = mm.count(p => p == '.')
  val t = mm.count(p => p == '|')
  val l = mm.count(p => p == '#')
  (o, t, l)
}

var stop = false
var i = 0L

// for part two I looked at the log; found the cycle and calculated the result

var empty = grid.clear

while (!stop) {
  println("-".repeat(10) + i + "-".repeat(10))
  grid.log()
  println("resourceValue: " + resourceValue(grid))

  var world = grid.delegate.keys
  world = world ++ world.flatMap(p => grid.neighbors8(p))

  var prime = empty
  world.foreach { e =>
    val (o, t, l) = ctx(grid)(grid.neighbors8(e))
    val pp = (grid(e.y, e.x), o, t, l) match {
      case ('.', _, tc, _) if tc >= 3 => '|'
      case ('.', _, _, _) => '.'
      case ('|', _, _, lc) if lc >= 3 => '#'
      case ('|', _, _, _) => '|'
      case ('#', _, tc, lc) if lc != 0 && tc != 0 => '#'
      case ('#', _, _, _) => '.'
    }
    if (pp != '.') {
      prime = prime.updated(e.y, e.x)(pp)
    }
  }
  grid = prime

  i = i + 1
  if (i > 10) {
    stop = true
  }
}
