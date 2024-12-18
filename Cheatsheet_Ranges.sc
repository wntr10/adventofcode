import $file.Grid_v3
import Grid_v3._

{
  val r0 = R(0, 10)
  println(r0)
  val r1 = R(-3, -1)
  println(r1)
  val i = r0.intersection(r1)
  println(i)
  val r2 = R(-3, 1)
  val i2 = r0.intersection(r2)
  println(i2)
  val s1 = r0.split
  println(s1)
  val s2 = r1.split
  println(s2)

}
