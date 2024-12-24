//import $file.^.Visu_v0
//import Visu_v0._

import $file.^.Basic
import Basic._
import Input._

//import java.io.File

val ex = ""

val inputRaw = read(s"day24$ex")
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
println(map)

var state = map("1").map {
  case s"$n: $b" => n -> b.toInt
}.toMap

case class GATE(a: String, b: String, c: String, f: Int)

val gates = map("2").map {
  case s"$a AND $b -> $c" => GATE(a, b, c, 1)
  case s"$a OR $b -> $c" => GATE(a, b, c, 2)
  case s"$a XOR $b -> $c" => GATE(a, b, c, 3)
}

/*
val vb = new VisuGraphBuilder
var nodes = Map.empty[String, VisuNode]
gates.zipWithIndex.foreach { g =>
  val co = g._1.f match {
    case 1 => VISU_BLUE
    case 2 => VISU_RED
    case 3 => VISU_GREEN
  }
  nodes = nodes.updated(g._1.c, vb.addNode(s"${g._1.c}(${g._2})", color = co))
}
val allg = gates.map(_.c).toSet

gates.foreach { g =>
  if (!allg.contains(g.a)) {
    nodes = nodes.updated(g.a, vb.addNode(g.a, color = VISU_BLACK))
  }
  if (!allg.contains(g.b)) {
    nodes = nodes.updated(g.b, vb.addNode(g.b, color = VISU_BLACK))
  }
}
gates.foreach { g =>
  vb.addLink(nodes(g.a), nodes(g.c))
  vb.addLink(nodes(g.b), nodes(g.c))
}
vb.build(new File("adder.graphml"))
*/


def getNumber(state: Map[String, Int], variable: String): String = {
  state.filter(_._1.startsWith(variable)).toVector.sortBy(_._1).reverse.map(_._2.toString).mkString
}

def check(swap: Set[Set[Int]], x: Long, y: Long): Boolean = {
  var statePrime = state

  def updateState(p: String, n: Long, size: Int): Unit = {
    val b = n.toBinaryString
    val bp = pad(b, size).reverse
    bp.zipWithIndex.foreach { r =>
      val k = p + pad(r._2.toString, 2)
      statePrime = statePrime.updated(k, r._1.toString.toInt)
    }
  }

  updateState("x", x, 45)
  updateState("y", y, 45)

  val bx = getNumber(statePrime, "x")
  val by = getNumber(statePrime, "y")
  val bz = getNumber(statePrime, "z")

  val (rx, ry, rz) = eval(swap, statePrime)

  val z = BigInt(bx, 2) + BigInt(by, 2)
  val z2 = BigInt(rz, 2)

  if (rx != bx || ry != by || z != z2) {
    println("====")
    println((bx, by, bz))
    println((rx, ry, rz))
    println(z.toString(2))
    println(z2.toString(2))
    false
  } else {
    true
  }
}

def eval(swap: Set[Set[Int]],
         _state: Map[String, Int]): (String, String, String) = {

  var gatesPrime = gates

  swap.foreach { s =>
    gatesPrime = gatesPrime.updated(s.head, gates(s.head).copy(c = gates(s.last).c))
    gatesPrime = gatesPrime.updated(s.last, gates(s.last).copy(c = gates(s.head).c))
  }

  var stop = false
  var state = _state
  var prime = state
  while (!stop) {
    gatesPrime.foreach {
      case GATE(a, b, c, 1) =>
        val va = state.getOrElse(a, 0)
        val vb = state.getOrElse(b, 0)
        prime = prime.updated(c, va & vb)
      case GATE(a, b, c, 2) =>
        val va = state.getOrElse(a, 0)
        val vb = state.getOrElse(b, 0)
        prime = prime.updated(c, va | vb)
      case GATE(a, b, c, 3) =>
        val va = state.getOrElse(a, 0)
        val vb = state.getOrElse(b, 0)
        prime = prime.updated(c, va ^ vb)
      case _ =>
    }
    if (prime == state) {
      stop = true
    }
    state = prime
  }

  (getNumber(state, "x"), getNumber(state, "y"), getNumber(state, "z"))
}

val (_, _, z) = eval(Set.empty, state)
println(s"PartOne: ${BigInt(z, 2)}")

// I printed the graph and fixed the logic

val swap = Set(Set(197, 152), Set(145, 82), Set(184, 5), Set(178, 120))

def run(): Unit = {
  Range(0, 45).foreach { p =>
    println(p)
    check(swap, 0L, 1L << p)
    check(swap, 1L << p, 0L)
    check(swap, 1L << p, 1L << p)
  }
}

var outputs = Vector.empty[String]
swap.foreach { s =>
  outputs = outputs :+ gates(s.head).c
  outputs = outputs :+ gates(s.last).c
}

println(outputs.sorted.mkString(","))
