import $file.^.Basic
import Basic._
import Input._

val ex = ".ex1" // 48
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
  var enabled = true

  prime.zipWithIndex.foreach {
    case (p, _) =>
      val s = splitOn("mul")(p)

      var sum = BigInt(0)

      def onOff(s: String): Unit = {
        val d = s.lastIndexOf("do()")
        val n = s.lastIndexOf("don't()")
        if (n != -1 && n > d) {
          enabled = false
        } else if (d != -1) {
          enabled = true
        }
      }

      s.foreach {
        case s"($a,$b)$rest" if enabled && a.forall(_.isDigit) && b.forall(_.isDigit) =>
          sum = sum + (BigInt(a) * BigInt(b))
          onOff(rest)
        case rest =>
          onOff(rest)
      }

      r = r + sum
  }
  r
}

println(run())
