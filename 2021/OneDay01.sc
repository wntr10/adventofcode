import $file.^.Basic, Basic._, Input._

val ex = ".ex0" // 7
val inputRaw = read(s"day01$ex")
val lines = split("\n", inputRaw)

type LINE = BigInt
var prime = Vector.empty[LINE]
var countRest = 0

def visit(line: String, idx: BigInt): Unit = {
  println(s"${pad(idx)}: <$line>")
  (line, idx) match {
    case (s"$str", _) =>
      prime = prime :+ BigInt(str)
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
  var window = Vector.empty[LINE]
  prime.zipWithIndex.foreach {
    case (p, _) =>
      window = p +: window
      if (window.length == 2 && window.head > window(1)) {
        r = r + 1
      }
      // time to live
      if (window.size == 2) {
        window = window.dropRight(1)
      }
  }
  r
}

println(run())
