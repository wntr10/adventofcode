import $ivy.`com.google.guava:guava:33.3.1-jre`
import $file.^.Basic, Basic._, Input._
import $file.^.Grid_v2, Grid_v2._

val ex = ".ex0" // 14
val inputRaw = read(s"day08$ex")
val lines = splitOn("\n")(inputRaw)

type LINE = Vector[Char]

var prime = Vector.empty[LINE]
var countRest = 0
var maxColumns = 0

def visit(line: String, idx: BigInt): Unit = {
  (line, idx) match {
    case (s"$str", _) =>
      val l = str.toVector
      prime = prime :+ l
      maxColumns = Math.max(maxColumns, l.length)
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

val grid = G(prime, maxColumns, '.').trim()

grid.log()

case class Antenna(id: Char, set: Set[BigPoint]) {

  def calc(): Set[BigPoint] = {

    var done = Set.empty[Set[BigPoint]]
    var re = Set.empty[BigPoint]

    set.foreach { a =>
      set.foreach { b =>
        if (a != b && !done.contains(Set(a, b))) {
          done = done + Set(a, b)

          val dx = b.x - a.x
          val dy = b.y - a.y

          val nna = P(a.x - dx, a.y - dy)
          val nnb = P(b.x + dx, b.y + dy)

          re = re ++ Set(nna, nnb).filter(an => grid.intersection(an.y, an.x))
        }
      }
    }

    re
  }
}

var antennaMap = Map.empty[Char, Antenna]

type RESULT = BigInt

def run(): RESULT = {

  grid.delegate.foreach { e =>
    val c = antennaMap.getOrElse(e._2, Antenna(e._2, Set.empty))
    antennaMap = antennaMap.updated(e._2, c.copy(set = c.set + e._1))
  }

  val antennas = antennaMap.values.toSet
  antennas.flatMap(_.calc()).size
}

println(run())
