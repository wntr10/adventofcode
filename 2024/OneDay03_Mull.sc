import $file.^.Basic
import Basic._
import Input._

val ex = ".ex0" // 161
val inputRaw = read(s"day03$ex")
val lines = splitOn("\n")(inputRaw)

type LINE = String
var prime = Vector.empty[LINE]
var countRest = 0

def visit(line: String, idx: BigInt): Unit = {
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

type RESULT = BigInt

def run(): RESULT = {
  var r: RESULT = 0
  prime.zipWithIndex.foreach {
    case (p, _) =>
      val s = splitOn("mul")(p).drop(1)

      var sum = BigInt(0)
      s.foreach {
        case s"($a,$b)$_" if a.forall(_.isDigit) && b.forall(_.isDigit) =>
          sum = sum + (BigInt(a) * BigInt(b))
        case _ =>
      }

      r = r + sum
  }
  r
}

println(run())
