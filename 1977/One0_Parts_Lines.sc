import $ivy.`org.jgrapht:jgrapht-core:1.5.2`
import $ivy.`com.google.guava:guava:33.3.1-jre`
import $file.^.Basic, Basic._, Input._

val ex = ".parts"

val inputRaw = read(s"day0$ex")

// Scala string interpolation cannot handle escaped characters
require(!inputRaw.contains("%"))
val input = inputRaw.replace('"', '%')

val parts = split("Part ", input)

//type LINE = BigInt
type LINE = String
var map = Map.empty[String, Vector[LINE]]
var countRest = 0

def visit(line: String, idx: BigInt, prime: Vector[LINE]): Vector[LINE] = {
  println(s"${pad(idx)}: <$line>")
  (line, idx) match {
    case (s"$str", _) =>
      prime :+ str
    case (l, i) =>
      countRest = countRest + 1
      println(s"REST ${pad(i)}: <$l>")
      prime
  }
}

parts.foreach { part =>
  val lines = split("\n", part)
  val id = lines.head.dropRight(1)
  println(s"--- $id ---")
  var prime = Vector.empty[LINE]

  lines.drop(1).zipWithIndex.foreach {
    case (lines, idx) =>
      prime = visit(lines, idx, prime)
  }

  println(prime.size)
  println(prime)
  //println(prime.sum)
  map = map.updated(id, prime)
}

require(countRest == 0)
println(map)
