import $file.^.Basic
import Basic._
import Input._
import $file.^.BigVector_v1
import BigVector_v1._
import scala.math.BigDecimal.RoundingMode

val ex = ".ex0" // 480; 875318608908

val inputRaw = read(s"day13$ex")
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

case class LOG(ax: Int, ay: Int, bx: Int, by: Int, px: Int, py: Int) {
  override def toString = s"Button A: X+$ax, Y+$ay\nButton B: X+$bx, Y+$by\nPrize: X=$px, Y=$py\n"
}

def runPart(part: Vector[LINE]): LOG = {

  var ax = 0
  var ay = 0
  var bx = 0
  var by = 0
  var px = 0
  var py = 0

  part.zipWithIndex.foreach {
    case (s"Button A: X+$x, Y+$y", _) =>
      ax = x.toInt
      ay = y.toInt
    case (s"Button B: X+$x, Y+$y", _) =>
      bx = x.toInt
      by = y.toInt
    case (s"Prize: X=$x, Y=$y", _) =>
      px = x.toInt
      py = y.toInt
    case r =>
      println(r)
      throw new RuntimeException()

  }
  LOG(ax, ay, bx, by, px, py)
}


def solve(a1: BigInt,
          b1: BigInt,
          c1: BigInt,
          a2: BigInt,
          b2: BigInt,
          c2: BigInt): BigInt = {

  def d(i: BigInt): BigDecimal = BigDecimal(i)

  val line1 = BigLine.fromPoints(BigVector.of(0, d(c1) / d(b1)), BigVector.of(d(c1) / d(a1), 0))
  val line2 = BigLine.fromPoints(BigVector.of(0, d(c2) / d(b2)), BigVector.of(d(c2) / d(a2), 0))

  line1.intersection(line2).map {
    case BigVector(na, nb) =>

      val nar = na.setScale(0, RoundingMode.HALF_UP)
      val nbr = nb.setScale(0, RoundingMode.HALF_UP)

      val ae = (na - nar).abs < BigLine.DEFAULT_TOLERANCE
      val be = (nb - nbr).abs < BigLine.DEFAULT_TOLERANCE

      if (ae && be) {
        (nar.toBigInt * 3) + nbr.toBigInt
      } else {
        BigInt(0)
      }
  }.getOrElse(BigInt(0))
}


def run(dd: BigInt): BigInt = {
  var r: BigInt = 0
  map.toVector.sortBy(_._1).foreach {
    case (_, part) =>
      val m = runPart(part)
      r += solve(m.ax, m.bx, dd + m.px, m.ay, m.by, dd + m.py)
  }
  r
}

println(run(0))
println(run(10000000000000L))
