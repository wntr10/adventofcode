import $file.^.Basic, Basic._, Input._
import $file.^.StringHelper_v1, StringHelper_v1._

val ex = ".ex0" // 142
val inputRaw = read(s"day01$ex")
val lines = split("\n", inputRaw)

type LINE = BigInt
var prime = Vector.empty[LINE]
var countRest = 0

def visit(line: String, idx: BigInt): Unit = {
  println(s"${pad(idx)}: <$line>")
  (line, idx) match {
    case (s"$str", _) =>
      val fi = StringHelper.firstDigit(str)
      val la = StringHelper.firstDigit(str.reverse)
      prime = prime :+ StringHelper.fromDigits(fi, la)
    case (l, i) =>
      countRest = countRest + 1
      println(s"REST ${pad(i)}: <$l>")
  }
}

lines.zipWithIndex.foreach {
  case (lines, idx) =>
    visit(lines, idx)
}

println(prime.sum)
