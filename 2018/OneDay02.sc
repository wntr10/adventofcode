import $ivy.`org.jgrapht:jgrapht-core:1.5.2`
import $ivy.`com.google.guava:guava:33.3.1-jre`
import $file.^.Basic, Basic._, Input._
import $file.^.Bags, Bags._

val ex = ".ex0" // 12
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

type RESULT = BigInt

def run(): RESULT = {
  var r2: RESULT = 0
  var r3: RESULT = 0
  prime.zipWithIndex.foreach {
    case (p, _) =>
      val b = Bag.of(p.toList)
      if (b.values().exists(v => v == 3)) {
        r3 = r3 + 1
      }

      if (b.values().exists(v => v == 2)) {
        r2 = r2 + 1
      }
  }
  r2 * r3
}

println(run())
