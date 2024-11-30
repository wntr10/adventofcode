import $ivy.`org.jgrapht:jgrapht-core:1.5.2`
import $ivy.`com.google.guava:guava:33.3.1-jre`
import $file.^.Basic, Basic._, Input._

val ex = ".parts1"

val inputRaw = read(s"day0$ex")

// Scala string interpolation cannot handle escaped characters
require(!inputRaw.contains("%"))
val input = inputRaw.replace('"', '%')

val parts = split("\n\n", input)
//val parts = split("Part ", input)

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

parts.zipWithIndex.foreach {
  case (part, pi) =>
    val lines = split("\n", part)

    val id = (pi + 1).toString
    //val id = lines.head.dropRight(1)
    println(s"--- $id ---")
    var prime = Vector.empty[LINE]

    lines.drop(0).zipWithIndex.foreach {
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

type RESULT_PART = BigInt

def runPart(part: Vector[LINE]): RESULT_PART = {
  var r: RESULT_PART = 0
  //var window = Vector.empty[LINE]
  part.zipWithIndex.foreach {
    case (p, _) =>
      r = r + p.size
      //window = p +: window
      //if (window.length == 2) {
        //r = r + 1
      //}
      // time to live
      //if (window.length == 2) {
        //window = window.dropRight(1)
      //}
  }
  r
}

type RESULT = BigInt

def run(): RESULT = {
  var r: RESULT = 0
  //var window = Vector.empty[RESULT_PART]
  map.foreach {
    case (id, part) =>
      val pr = runPart(part)
      r = r + pr
      //window = pr +: window
      //if (window.length == 2) {
        //r = r + 1
      //}
      // time to live
      //if (window.length == 2) {
        //window = window.dropRight(1)
      //}
  }
  r
}

println(run())
