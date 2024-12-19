import $file.^.Basic
import Basic._
import Input._

val ex = ".ex0" // 6; 16

val inputRaw = read(s"day19$ex")
val parts = split("\n\n", inputRaw)
type LINE = String
var map = Map.empty[String, Vector[LINE]]
var countRest = 0

def visit(line: String, idx: BigInt, prime: Vector[LINE]): Vector[LINE] = {
  (line, idx) match {
    case (s"$str", _) =>
      prime :+ str
    case (l, i) =>
      countRest = countRest + 1
      println(s"REST ${pad(i)}: <$l>")
      prime
  }
}

parts.zipWithIndex.foreach {
  case (part, pi) =>
    val lines = split("\n", part)

    val id = (pi + 1).toString
    var prime = Vector.empty[LINE]

    lines.drop(0).zipWithIndex.foreach {
      case (lines, idx) =>
        prime = visit(lines, idx, prime)
    }

    map = map.updated(id, prime)
}

require(countRest == 0)

val pattern = splitOn(",")(map("1").head)
println(pattern.mkString("(", "|", ")"))

val designs = map("2")

def check(str: String): BigInt = {

  var rest = Set((str, BigInt(1)))

  var cn = BigInt(0)

  while (rest.nonEmpty) {
    var prime = Map.empty[String, BigInt]
    rest.foreach { r =>
      pattern.zipWithIndex.foreach { p =>
        val isPrefix = r._1.startsWith(p._1)
        if (isPrefix) {
          r._1.drop(p._1.length) match {
            case "" =>
              cn = cn + r._2
            case n =>
              val cc = prime.getOrElse(n, BigInt(0))
              prime = prime.updated(n, cc + r._2)
          }
        }
      }
    }
    rest = prime.toSet
  }
  cn
}

var ways = BigInt(0)
var success = BigInt(0)

designs.foreach { l =>
  val w = check(l)
  if (w != 0) {
    success += 1
  }
  ways += w
}

println(success)
println(ways)
