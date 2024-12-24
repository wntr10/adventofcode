import $file.^.Basic
import Basic._
import Input._

val ex = ".ex0" // 7; co,de,ka,ta

val inputRaw = read(s"day23$ex")
val lines = splitOn("\n")(inputRaw)

type LINE = (String, String)
var prime = Vector.empty[LINE]
var countRest = 0

def visit(line: String, idx: BigInt): Unit = {
  (line, idx) match {
    case (s"$a-$b", _) =>
      val l = (a, b)
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

val ps = prime.toSet
def connected(a: String)(b: String): Boolean = {
  ps.contains((a, b)) || ps.contains((b, a))
}

def neighborSet(a: String): Set[String] = {
  ps.filter(_._1 == a).map(_._2) ++ ps.filter(_._2 == a).map(_._1)
}

val comps = prime.flatMap(t => Set(t._1, t._2)).toSet

var partOne = 0
def visit(s: Set[String]): Unit = {
  partOne += 1
}

var partTwo = Set.empty[String]
def visitTwo(s: Set[String]): Unit = {
  if (s.size > partTwo.size) {
    partTwo = s
  }
}

def subSets(visit: Set[String] => Unit)
           (r: Set[String], p: Set[String]): Unit = {

  if (r.size == 3) {
    visit(r)
  } else if (p.nonEmpty) {
    val pick = p.head
    if (r.forall(connected(pick)) && (r.size != 2 || (r + pick).exists(_.startsWith("t")))) {
      subSets(visit)(r + pick, p.drop(1))
    }
    subSets(visit)(r, p.drop(1))
  }
}

def bronKerbosch(visit: Set[String] => Unit)
                (r: Set[String], _p: Set[String], _x: Set[String]): Unit = {

  if (_p.isEmpty && _x.isEmpty) {
    visit(r)
  } else {
    var p = _p
    var x = _x
    p.foreach { v =>
      val n = neighborSet(v)
      bronKerbosch(visit)(r + v, p.intersect(n), x.intersect(n))
      p -= v
      x += v
    }
  }
}

subSets(visit)(Set.empty, comps)
println(partOne)

bronKerbosch(visitTwo)(Set.empty, comps, Set.empty)
println(partTwo.toVector.sorted.mkString(","))
