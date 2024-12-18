import $file.^.Basic, Basic._, Input._
import $file.^.Grid_v3, Grid_v3._
import $file.^.Voxel_v1, Voxel_v1._
import $file.^.BigVector_v1
import BigVector_v1._

//val ex = ".ex0" // best location: P(3,4,0,0) detect 8 asteroids
//val ex = ".ex1" // best location: P(5,8,0,0) detect 33 asteroids
//val ex = ".ex2" // best location: P(1,2,0,0) detect 35 asteroids
//val ex = ".ex3" // best location: P(6,3,0,0) detect 41 asteroids
val ex = ".ex4" // best location: P(11,13,0,0) detect 210 asteroids; 200th: P(8,2,0,0); 802

val inputRaw = read(s"day10$ex")
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

var gp = grid.clear
var asteroids = grid.delegate.keySet

var best = P()
var bestDetect = BigInt(0)

asteroids.foreach { a =>
  var detect = 0
  asteroids.foreach { b =>

    var blocked = false

    def visit(x: BigInt, y: BigInt): Boolean = {
      val p = P(x, y)

      if (p == a || p == b) {
        true
      } else if (asteroids.contains(p)) {
        val lineAB = BigLine.fromPoints(BigVector.of(a.x.toInt, a.y.toInt), BigVector.of(b.x.toInt, b.y.toInt))
        blocked = lineAB.contains(BigVector.of(x.toInt, y.toInt))
        !blocked
      } else {
        true
      }
    }

    if (a != b) {
      Voxel.traverse(visit)(a.x, a.y, b.x, b.y)
      if (!blocked) {
        detect = detect + 1
      }
    }
  }
  if (detect > bestDetect) {
    bestDetect = detect
    best = a
  }
  gp = gp.updated(a.y, a.x)(detect.toString.charAt(0))
}

gp.log()

println(s"best location: $best detect $bestDetect asteroids")

val laser = best

var asteroidsSet = grid.delegate.keys.filter(a => a != laser).toVector.map(p => ASTEROID(laser, p))

def normAngle(a: Double): Double = {
  var p = a
  if (p < 0.0) {
    p = p + 360.0
  }
  p
}

def manhattan(a: P, b: P): BigInt = {
  (b.y - a.y).abs + (b.x - a.x).abs
}

final case class ASTEROID(laser: P, p: P) {
  val line = BigLine.fromPoints(BigVector.of(laser.x.toInt, laser.y.toInt), BigVector.of(p.x.toInt, p.y.toInt))
  val up = BigLine.fromPoints(BigVector.of(laser.x.toInt, laser.y.toInt), BigVector.of(laser.x.toInt, 0))
  val angle = normAngle(up.angle.toDouble.toDegrees - line.angle.toDouble.toDegrees)
  val dist = manhattan(laser, p)

  override def toString = s"A($p,angle=$angle,dist=$dist)"
}

val ordering = new Ordering[ASTEROID]() {
  override def compare(x: ASTEROID, y: ASTEROID): Int = {
    val c = x.angle.compareTo(y.angle)
    if (c == 0) {
      x.dist.compareTo(y.dist)
    } else {
      c
    }
  }
}

var vaporized = List.empty[ASTEROID]

while (asteroidsSet.nonEmpty && vaporized.size < 200) {
  var angle = -1.0
  asteroidsSet.sorted(ordering).foreach { a =>
    if (a.angle > angle) {
      vaporized = a :: vaporized
      if (vaporized.size == 200) {
        val score = vaporized.head.p.x * 100 + vaporized.head.p.y
        println(s"200th: ${vaporized.head.p} = $score")
      }
      angle = a.angle
    }
  }
  asteroidsSet = asteroidsSet.filter(a => !vaporized.contains(a))
}
