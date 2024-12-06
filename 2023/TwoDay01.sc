import $file.^.Basic, Basic._, Input._
import $file.^.StringHelper_v1, StringHelper_v1._

val ex = ".ex1" // 281
val inputRaw = read(s"day01$ex")
val lines = split("\n", inputRaw)

type LINE = BigInt
var prime = Vector.empty[LINE]
var countRest = 0

def visit(line: String, idx: BigInt): Unit = {
  println(s"${pad(idx)}: <$line>")
  (line, idx) match {
    case (s"$str", _) =>
      val both = StringHelper.DIGITS.values.flatMap(_.toSet).toSet
      val fi = StringHelper.toBigInt(StringHelper.firstMatch(str, both).get)
      val la = StringHelper.toBigInt(StringHelper.firstMatch(str.reverse, both.map(_.reverse)).get.reverse)
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
