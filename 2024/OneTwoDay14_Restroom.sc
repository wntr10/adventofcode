import $file.^.Basic
import Basic._
import Input._
import $file.^.Grid_v3
import Grid_v3._
import $file.^.BigIntHelper_v1
import BigIntHelper_v1.BigIntHelper._

val ex = ".ex0" // 12
val inputRaw = read(s"day14$ex")
val lines = splitOn("\n")(inputRaw)

case class ROBOT(x: Int, y: Int, vx: Int, vy: Int)

type LINE = ROBOT

var prime = Vector.empty[LINE]
var countRest = 0

def visit(line: String, idx: BigInt): Unit = {
  (line, idx) match {
    case (s"p=$x,$y v=$vx,$vy", _) =>
      val l = ROBOT(x.toInt, y.toInt, vx.toInt, vy.toInt)
      prime = prime :+ l
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

val maxX = 11
val maxY = 7

//val maxX = 101
//val maxY = 103

def show(maxX: Int, maxY: Int)(robots: Set[LINE]): Unit = {
  var map = Map.empty[P, Char]
  Range(0, maxY).foreach { y =>
    Range(0, maxX).foreach { x =>
      val c = robots.filter(p => p.x == x && p.y == y)
      val v = if (c.nonEmpty) {
        c.size.toString.charAt(0)
      } else {
        '.'
      }
      map = map.updated(P(x, y), v)
    }
  }
  G(map, vec(maxY, maxX), '.').log()
}

def step(maxX: Int, maxY: Int)(robots: Set[LINE]): Set[LINE] = {
  var nxt = Set.empty[ROBOT]

  robots.foreach { p =>
    var np = p.copy(x = p.x + p.vx, y = p.y + p.vy)
    if (np.x < 0) {
      np = np.copy(x = maxX + np.x)
    } else if (np.x >= maxX) {
      np = np.copy(x = np.x - maxX)
    }
    if (np.y < 0) {
      np = np.copy(y = maxY + np.y)
    } else if (np.y >= maxY) {
      np = np.copy(y = np.y - maxY)
    }
    nxt = nxt + np
  }

  nxt
}

def safetyFactor(maxX: Int, maxY: Int)(robots: Set[LINE]): BigInt = {
  val midX = maxX >> 1
  val midY = maxY >> 1

  val q1: BigInt = robots.count(p => p.x < midX && p.y < midY)
  val q2: BigInt = robots.count(p => p.x > midX && p.y < midY)
  val q3: BigInt = robots.count(p => p.x < midX && p.y > midY)
  val q4: BigInt = robots.count(p => p.x > midX && p.y > midY)

  q1 * q2 * q3 * q4
}

def run(robots: Set[LINE], maxSeconds: Int): RESULT = {

  var current = robots
  var seconds = 0

  while (seconds < maxSeconds) {

    current = step(maxX, maxY)(current: Set[LINE])
    val points = current.map(robot => (robot.x, robot.y))

    seconds = seconds + 1
    if (current.size == points.size) {
      show(maxX, maxY)(current)
      println("Easter egg at " + seconds)
    }
  }

  safetyFactor(maxX, maxY)(current)
}

println(run(prime.toSet, 100))
//println(run(prime.toSet, 10402))
