import $file.^.Basic, Basic._, Input._

val ex = ".ex1" // fgij
val inputRaw = read(s"day02$ex")
val lines = split("\n", inputRaw)

type LINE = String
var prime = Vector.empty[LINE]
var countRest = 0

def visit(line: String, idx: BigInt): Unit = {
  println(s"${pad(idx)}: <$line>")
  (line, idx) match {
    case (s"$str", _) =>
      prime = prime :+ str
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

def and(a: String, b: String): String = {
  require(a.length == b.length)
  var r = ""
  a.zip(b).foreach {
    case (ac, bc) if ac != bc =>
    // skip
    case (a, _) =>
      r = r + a
  }
  r
}

type RESULT = String

def run(): RESULT = {
  prime.foreach { a =>
    prime.foreach { b =>
      val d = and(a, b)
      if (d.length == a.size - 1) {
        return d
      }
    }
  }
  ""
}

println(run())
