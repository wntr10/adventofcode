import $file.BigVector_v1
import BigVector_v1._
import $file.BigIntHelper_v1
import BigIntHelper_v1.BigIntHelper._
import $file.Grid_v3
import Grid_v3._
import $file.Voxel_v1, Voxel_v1._

import scala.util.Random


{
  val w = 100
  val h = 60
  val my = h >> 1
  val mx = w >> 1
  val center = P(mx, my)


  var o = G.empty(vec(h, w), '.')
  println(o)
  o.log()
  val rnd = Random
  val x1 = rnd.nextInt(w)
  val y1 = rnd.nextInt(h)
  val x2 = rnd.nextInt(w)
  val y2 = rnd.nextInt(h)
  val x3 = rnd.nextInt(w)
  val y3 = rnd.nextInt(h)
  val x4 = rnd.nextInt(w)
  val y4 = rnd.nextInt(h)

  val p1 = P(x1, y1)
  val p2 = P(x2, y2)
  val p3 = P(x3, y3)
  val p4 = P(x4, y4)
  o = o.updated(p1.y, p1.x)('A')
  o = o.updated(p2.y, p2.x)('B')
  o = o.updated(p3.y, p3.x)('C')
  o = o.updated(p4.y, p4.x)('D')
  o = o.updated(center.y, center.x)('X')
  o.logWithColors(Set(p1, p2, p3, p4, center))
  //val lineAB = BigLine.fromPoints(BigVector.of(x1, y1), BigVector.of(x2, y2))
  //val lineCD = BigLine.fromPoints(BigVector.of(x3, y3), BigVector.of(x4, y4))
  //val is = lineAB.intersection(lineCD)
  //is.foreach { p =>
    //o = o.updated(p.y.toBigInt, p.x.toBigInt)('X')
    //o = o.updated(p.y.rounded.toBigInt, p.x.rounded.toBigInt)('@')
  //}
  //o.log()

  //val zero = BigDecimal(0)
  //for (y <- 0 until 20) {
    //for (x <- 0 until 20) {
      //if (lineAB.contains(BigVector.of(x, y))) {
        //o = o.updated(y, x)('o')
      //}
    //}
  //}
  //o.log()

  var set = Set.empty[P]
  var d = 0
  var tx = 0
  var ty = 0
  var stop = false
  while(!stop) {
  if (d == 0) {
      tx = tx + 1
      if (tx < o.shape(1)) {
        set = set + P(tx, ty)
      } else {
        tx = tx - 1
        d = 1
      }
    } else if (d == 1) {
      ty = ty + 1
      if (ty < o.shape(0)) {
        set = set + P(tx, ty)
      } else {
        ty = ty - 1
        d = 2
      }
    } else if (d == 2) {
      tx = tx - 1
      if (tx >= 0) {
        set = set + P(tx, ty)
      } else {
        tx = tx + 1
        d = 3
      }
    } else if (d == 3) {
      ty = ty - 1
      if (ty >= 0) {
        set = set + P(tx, ty)
      } else {
        ty = ty + 1
        d = 0
      }
    }
    if (tx == 0 && ty == 0) {
      stop = true
    }
  }

  def normAngle(a: Double): Double = {
    var p = a
    if (p < 0.0) {
      p = p + 360.0
    }
    p
  }


  var fill = Set.empty[P]
  var fill2 = Set.empty[P]

  set.take(10).foreach { s =>
    def visit(x: BigInt, y: BigInt): Boolean = {
      fill = fill + P(x, y)

      val line = BigLine.fromPoints(BigVector.of(center.x.toInt, center.y.toInt), BigVector.of(s.x.toInt, s.y.toInt))
      println(line.angle)
      println(line.angle.toDouble.toDegrees)
      println(normAngle(line.angle.toDouble.toDegrees))

      val offset = line.offset(BigVector.of(x.toInt, y.toInt))
      if (offset > -0.5 && offset <= 0.5) {
        fill2 = fill2 + P(x, y)
      }


      true
    }

    Voxel.traverse(visit)(center.x, center.y, s.x, s.y)
  }


  println(set.size)
  o.logWithColors(Set.empty, set, fill, fill2)

}
