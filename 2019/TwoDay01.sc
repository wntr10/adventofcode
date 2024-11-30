import $file.^.Basic
import Basic._
import Input._

import scala.annotation.tailrec

val ex = ".ex0"
val inputRaw = read(s"day01$ex")
val lines = split("\n", inputRaw)

type LINE = BigInt
var prime = Vector.empty[LINE]
var countRest = 0

@tailrec
def fuel(m: BigInt, total: BigInt): BigInt = {
  val f = (m / 3) - 2
  if (f <= 0) {
    total
  } else {
    fuel(f, total + f)
  }
}

def visit(line: String, idx: BigInt): Unit = {
  println(s"${pad(idx)}: <$line>")
  (line, idx) match {
    case (s"$str", _) =>
      val f = fuel(BigInt(str), 0)
      println(f)
      prime = prime :+ f
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
println(prime.sum)
