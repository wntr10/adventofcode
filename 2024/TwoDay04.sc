import $file.^.Basic, Basic._, Input._
import $file.^.StringHelper_v1, StringHelper_v1._
import $file.^.Grid_v3, Grid_v3._

val ex = ".ex0" // 9
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

val grid = G(prime, maxColumns, '_')

type RESULT = BigInt

def run(): RESULT = {
  var r: RESULT = 0

  def findX(x: Int, y: Int): Unit = {
    val s = Set(
      "MSAMS",
      "SMASM",
      "SSAMM",
      "MMASS"
    )
    s.foreach { e =>
      if (find(x, y, e)) {
        r = r + 1
      }
    }
  }

  def find(x: Int, y: Int, str: String): Boolean = {
    if (x + 2 >= grid.shape.head) return false
    if (y + 2 >= grid.shape.last) return false
    grid(y, x) == str.charAt(0) &&
      grid(y + 2, x) == str.charAt(1) &&
      grid(y + 1, x + 1) == str.charAt(2) &&
      grid(y, x + 2) == str.charAt(3) &&
      grid(y + 2, x + 2) == str.charAt(4)
  }

  Range(0, grid.shape.last.toInt).foreach { y =>
    Range(0, grid.shape.head.toInt).foreach { x =>
      findX(x, y)
    }
  }

  r
}

println(run())
