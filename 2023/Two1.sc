import $ivy.`org.jgrapht:jgrapht-core:1.5.2`
import $ivy.`com.google.guava:guava:33.3.1-jre`
import $file.^.Basic, Basic._, Input._

val ex = ".ex1" // 281
val inputRaw = read(s"day01$ex")

// Scala string interpolation cannot handle escaped characters
require(!inputRaw.contains("%"))
val input = inputRaw.replace('"', '%')
val lines = split("\n", input)

type LINE = BigInt
var prime = Vector.empty[LINE]
var countRest = 0

def visit(line: String, idx: BigInt): Unit = {
  println(s"${pad(idx)}: <$line>")
  (line, idx) match {
    case (s"$str", _) =>
      val both = DIGITS.values.flatMap(_.toSet).toSet
      val fi = toBigInt(firstMatch(str, both).get)
      val la = toBigInt(firstMatch(str.reverse, both.map(_.reverse)).get.reverse)
      prime = prime :+ fromDigits(fi, la)
    case (l, i) =>
      countRest = countRest + 1
      println(s"REST ${pad(i)}: <$l>")
  }
}

lines.zipWithIndex.foreach {
  case (lines, idx) =>
    visit(lines, idx)
}

println(prime.size)
println(prime.sum)