import $file.^.Basic, Basic._, Input._

val ex = ".ex0" // 18
val inputRaw = read(s"day02$ex")
val lines = split("\n", inputRaw)

type LINE = Set[BigInt]
var prime = Vector.empty[LINE]
var countRest = 0

def visit(line: String, idx: BigInt): Unit = {
  println(s"${pad(idx)}: <$line>")
  (line, idx) match {
    case (s"$str", _) =>
      val l = split("\t", str).map(n => BigInt(n)).toSet
      prime = prime :+ l
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

type RESULT = BigInt

def run(): RESULT = {
  var r: RESULT = 0
  prime.zipWithIndex.foreach {
    case (p, _) =>
      r = r + (p.max - p.min)
  }
  r
}

println(run())
