import $file.^.Basic, Basic._, Input._
import $file.^.StringHelper_v1, StringHelper_v1._
import $file.^.Grid_v1, Grid_v1._

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

val grid = G(prime, maxColumns, '_')

type RESULT = BigInt

def run(): RESULT = {
  var r: RESULT = 0

  def search(s: String): Unit = {
    r = r + StringHelper.count(s, "XMAS")
    r = r + StringHelper.count(s, "XMAS".reverse)
  }

  grid.rows.foreach(search)
  grid.columns.foreach(search)
  grid.diagonal1.foreach(search)
  grid.diagonal2.foreach(search)

  r
}

println(run())
