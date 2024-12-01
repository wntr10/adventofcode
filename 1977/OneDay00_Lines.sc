import $ivy.`org.jgrapht:jgrapht-core:1.5.2`
import $ivy.`com.google.guava:guava:33.3.1-jre`
import $file.^.Basic, Basic._, Input._
import $file.^.Bags, Bags._

val ex = ".lines"
val inputRaw = read(s"day0$ex")

// Scala string interpolation cannot handle escaped characters
require(!inputRaw.contains("%"))
val input = inputRaw.replace('"', '%')
val lines = split("\n", input)

//type LINE = BigInt
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
println(prime.size)
println(prime)
//println(prime.sum)

type RESULT = BigInt

def run(): RESULT = {
  var r: RESULT = 0
  //var window = Vector.empty[LINE]
  prime.zipWithIndex.foreach {
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

println(run())
