import $file.BigVector_v1
import BigVector_v1._
import $file.BigIntHelper_v1
import BigIntHelper_v1.BigIntHelper._
import $file.Grid_v2
import Grid_v2._

import scala.util.Random


{
  var o = G.empty(vec(20, 20), '.')
  println(o)
  o.log()
  val rnd = Random
  val x1 = rnd.nextInt(20)
  val y1 = rnd.nextInt(20)
  val x2 = rnd.nextInt(20)
  val y2 = rnd.nextInt(20)
  val x3 = rnd.nextInt(20)
  val y3 = rnd.nextInt(20)
  val x4 = rnd.nextInt(20)
  val y4 = rnd.nextInt(20)

  val p1 = P(x1, y1)
  val p2 = P(x2, y2)
  val p3 = P(x3, y3)
  val p4 = P(x4, y4)
  o = o.updated(p1.y, p1.x)('A')
  o = o.updated(p2.y, p2.x)('B')
  o = o.updated(p3.y, p3.x)('C')
  o = o.updated(p4.y, p4.x)('D')
  o.log()
  val lineAB = BigLine.fromPoints(BigVector.of(x1, y1), BigVector.of(x2, y2))
  val lineCD = BigLine.fromPoints(BigVector.of(x3, y3), BigVector.of(x4, y4))
  val is = lineAB.intersection(lineCD)
  is.foreach { p =>
    o = o.updated(p.y.toBigInt, p.x.toBigInt)('X')
    o = o.updated(p.y.rounded.toBigInt, p.x.rounded.toBigInt)('@')
  }
  o.log()



}
