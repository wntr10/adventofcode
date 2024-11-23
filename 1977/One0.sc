import $ivy.`org.jgrapht:jgrapht-core:1.5.2`
import $ivy.`com.google.guava:guava:33.3.1-jre`
import $file.^.Basic, Basic._, Input._

val ex = ""

val inputRaw = read(s"day0$ex")
//val input = inputRaw
val input = inputRaw.replace('"', '%')

val lines = split("\n", input)

var rest = 0
lines.foreach {
  case s"$str" =>
    println(str)
  case r =>
    rest = rest + 1
    println(s"!<$r>")
}

require(rest == 0, rest)

