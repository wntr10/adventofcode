import $ivy.`org.jgrapht:jgrapht-core:1.5.2`
import $ivy.`com.google.guava:guava:33.3.1-jre`
import $file.^.Basic, Basic._, Input._
import $file.^.Bag_v1, Bag_v1._
import $file.^.StringHelper_v1, StringHelper_v1._
import $file.^.Grid_v3, Grid_v3._
import $file.^.BigIntHelper_v1, BigIntHelper_v1.BigIntHelper.vec

val ex = ".line"
val inputRaw = read(s"day0$ex")

// Scala string interpolation cannot handle escaped characters
require(!inputRaw.contains("%"))
val input = inputRaw.replace('"', '%')
val line = splitOn("\n")(input).head

//type LINE = BigInt
type LINE = Vector[String]
//type LINE = String

var countRest = 0

def visit(line: String): LINE = {
  println(s"<$line>")
  line match {
    case s"$str" =>
      splitOn(",")(str).toVector
    case l =>
      countRest = countRest + 1
      println(s"REST <$l>")
      throw new RuntimeException()
  }
}

val prime = visit(line)

type RESULT = BigInt

def run(): RESULT = {
  var r: RESULT = 0
  r = prime.size
  r
}

println(run())
