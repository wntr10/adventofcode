import $ivy.`org.jgrapht:jgrapht-core:1.5.2`
import $ivy.`com.google.guava:guava:33.3.1-jre`
import $file.^.Basic, Basic._, Input._
import $file.^.Grid_v0, Grid_v0._

val ex = ".grid"
val inputRaw = read(s"day0$ex")

// Scala string interpolation cannot handle escaped characters
require(!inputRaw.contains("%"))
val input = inputRaw.replace('"', '%')
val lines = split("\n", input)


case class POINT(x: BigInt, y: BigInt) extends Point {
  override def of(ax: BigInt, ay: BigInt) = POINT(ax, ay)

  override def withXY(x: BigInt, y: BigInt) = this.copy(x = x, y = y)
}

type VALUE = Char
var map = Map.empty[POINT, VALUE]
var countRest = 0

def visit(line: String, idx: BigInt): Unit = {
  println(s"${pad(idx)}: <$line>")
  (line, idx) match {
    case (s"$str", y) =>
      //val columns = split("\t", str)
      val columns = str
      columns.zipWithIndex.foreach {
        case (c, x) =>
          map = map.updated(POINT(x, y), c)
        case _ =>
        // skip
      }
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

Grid.log(map, '.')
