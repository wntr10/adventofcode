import $file.BigIntHelper_v1
import BigIntHelper_v1.BigIntHelper._
import $file.Grid_v2
import Grid_v2._


{
  val o = G.empty(vec(9, 5), '_')
  var g = o
  val g6 = G.empty(vec(2, 5), '%').fill()
  val g2 = G.empty(vec(2, 2), '$')
  val g3 = g || g2.fill()
  println(g)
  g.log()
  g = g.updated(1)('#')
  g = g.updated(44)('@')
  g.log()
  println(s"(0,0)=${g(0, 0)}")
  println(s"(20,10)=${g(20, 10)}")
  println(s"(44)=${g(44)}")

  g2.log()

  println(g3)
  g3.log()

  val r0 = R(1, 8)
  val r1 = R(0, 4)
  val g4 = g3.slice(r0, r1)

  println(g4)
  g4.log()

  println(o)
  o.log()

  println(g6)
  g6.log()


  val g5 = o.concatenate(g6)

  println(g5)
  g5.log()


}


{
  println("=====")
  val p0 = P(1)
  println(p0)
  val p1 = P(0)
  println(p1)
  println(p1.component(0))
  println(p1.component(1))
  println(p1.component(2))
  println(p1.add())
  println(p1.add(3))
  println(p1.add(4, 3))
  println(p1.add(5, 4, 3))
  println(p1.add(6, 5, 4, 3))

  var g = G.empty(vec(3, 2), BigInt(0))
  Range(0, g.size.toInt).foreach { v =>
    g = g.updated(v)(v)
  }

  println(g)
  println(g.delegate)
  g.log()

  println(g(0))
  println(g(4))
  println(g(1, 1))

  var h = G.empty(vec(3, 1), BigInt(0))
  Range(0, h.size.toInt).foreach { v =>
    h = h.updated(v)(v + 6)
  }

  println(h)
  println(h.delegate)
  h.log()

  val c = g.concatenate(h, axis = 1)

  println(c)
  println(c.delegate)
  c.log()


}

{
  var g = G.empty(vec(9), BigInt(0))
  Range(0, g.size.toInt).foreach { v =>
    g = g.updated(v)(v)
  }
  println(g)
  g.log()
  println(g(0))
  println(g(8))
  println(g(-1))
}

{
  var g = G.empty(vec(2, 9), BigInt(0))
  Range(0, g.size.toInt).foreach { v =>
    g = g.updated(v)(v)
  }
  println(g)
  g.log()
  println(g(0))
  println(g(8))
  println(g(17))
  println(g(-1))
}
