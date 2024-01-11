package wntr10.adventofcode.y2023.d24

import wntr10.adventofcode.Input

object PartTwo extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.parse(lines)
  }

  lazy val length: Int = input.length

  case class Hailstone(px: Double,
                       py: Double,
                       pz: Double,
                       vx: Double,
                       vy: Double,
                       vz: Double) {

    val px2: Double = px + vx
    val py2: Double = py + vy
    val pz2: Double = pz + vz

    val position: MyVector3D = {
      val p1 = MyVector3D(px, py, pz)
      val p2 = MyVector3D(px2, py2, pz2)
      p1.vectorTo(p2)
    }
  }

  private def solve(input: Parser.Alpha): Unit = {
    val hs = input.map { l =>
      Hailstone(l.a.head.doubleValue(), l.a(1).doubleValue(), l.a.last.doubleValue(), l.b.head.doubleValue(), l.b(1).doubleValue(), l.b.last.doubleValue())
    }

    println("from z3 import *")
    println("s = Solver()")
    println("x1 = Real('x1')")
    println("y1 = Real('y1')")
    println("z1 = Real('z1')")

    println("dx1 = Real('dx1')")
    println("dy1 = Real('dy1')")
    println("dz1 = Real('dz1')")

    hs.take(3).zipWithIndex.foreach { hz =>
      val h = hz._1
      println(s"t${hz._2} = Real('t${hz._2}')")
      println(s"s.add((${h.px} + t${hz._2} * ${h.position.x}) - (x1 + t${hz._2} * dx1) == 0)")
      println(s"s.add((${h.py} + t${hz._2} * ${h.position.y}) - (y1 + t${hz._2} * dy1) == 0)")
      println(s"s.add((${h.pz} + t${hz._2} * ${h.position.z}) - (z1 + t${hz._2} * dz1) == 0)")
    }

    println("s.check()")
    println("s.model()")
  }

  println(solve(input))
}
