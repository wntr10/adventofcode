import $file.^.Basic
import Basic._
import Input._
import $file.^.Grid_v3
import Grid_v3._
import $file.^.BigIntHelper_v1, BigIntHelper_v1.BigIntHelper._

val ex = ".ex0" // 1930, 1206
val inputRaw = read(s"day12$ex")
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

val grid = G(prime, maxColumns, '.')
grid.log()

def flood(p: P, c: Char, done: Set[P] = Set.empty): Set[P] = {
  var current = done + p
  vec(-1, 0, 1).foreach { dy =>
    vec(-1, 0, 1).foreach { dx =>
      if (dy.abs != dx.abs) {
        val n = p.add(dy, dx)
        if (!current.contains(n) && grid.getOrElseZero(n) == c) {
          current = flood(n, c, current)
        }
      }
    }
  }
  current
}

def perimeter(region: Set[P]): BigInt = {
  var a = BigInt(0)
  region.foreach { p =>
    var b = 4
    vec(-1, 0, 1).foreach { dy =>
      vec(-1, 0, 1).foreach { dx =>
        if (dy.abs != dx.abs) {
          val n = p.add(dy, dx)
          if (region.contains(n)) {
            b -= 1
          }
        }
      }
    }
    a += b
  }
  a
}

def numberOfSides(r: Set[P]): Map[P, Set[Int]] = {
  var a = Map.empty[P, Set[Int]]
  r.foreach { u =>
    var b = Set.empty[Int]
    var idx = 0
    vec(-1, 0, 1).foreach { dy =>
      vec(-1, 0, 1).foreach { dx =>
        if (dy.abs != dx.abs) {
          val n = u.add(dy, dx)
          if (!r.contains(n)) {
            b += idx
          }
          idx += 1
        }
      }
    }
    if (b.nonEmpty) {
      a = a.updated(u, b)
    }
  }
  a
}

def group(s: Set[BigInt]): BigInt = {
  val sl = s.toList.sorted
  var gaps = BigInt(1)
  var last = sl.head
  sl.drop(1).foreach { n =>
    if (n - last > 1) {
      gaps += 1
    }
    last = n
  }
  gaps
}

def distinctHorizontal(set: Set[P]): BigInt = {
  var lines = Map.empty[BigInt, Set[P]]
  set.foreach { s =>
    var cl = lines.getOrElse(s.y, Set.empty)
    cl += s
    lines = lines.updated(s.y, cl)
  }
  var si = BigInt(0)
  lines.foreach { e =>
    val s = e._2.map(_.x)
    si += group(s)
  }
  si
}

def distinctVertical(set: Set[P]): BigInt = {
  var columns = Map.empty[BigInt, Set[P]]
  set.foreach { s =>
    var cl = columns.getOrElse(s.x, Set.empty)
    cl += s
    columns = columns.updated(s.x, cl)
  }
  var si = BigInt(0)
  columns.foreach { e =>
    val s = e._2.map(_.y)
    si += group(s)
  }
  si
}

def sides(r: Set[P]): BigInt = {
  val p2 = numberOfSides(r)
  val s = p2.values.toSet.flatten
  var ccs = BigInt(0)
  s.foreach { n =>
    var set = Set.empty[P]
    p2.foreach { e =>
      if (e._2.contains(n)) {
        set += e._1
      }
    }
    n match {
      case d if Set(0, 3).contains(d) =>
        ccs += distinctHorizontal(set)
      case d if Set(1, 2).contains(d) =>
        ccs += distinctVertical(set)
    }
  }
  ccs
}

var regions = Set.empty[Set[P]]
var all = Set.empty[P]

require(grid.delegate.keySet.size == (grid.shape(0) * grid.shape(1)))

grid.delegate.keySet.foreach { k =>
  if (!all.contains(k)) {
    val r = flood(k, grid.getOrElseZero(k))
    all ++= r
    regions += r
  }
}

grid.logWithColors(regions.toSeq: _*)

println("number-of-regions: " + regions.size)

var sumPeri = BigInt(0)
println("====")
regions.foreach { r =>
  val c = grid.get(r.head)
  val area = r.size
  val peri = perimeter(r)
  val price = area * peri
  println(s"A region of <$c> plants with price $area * $peri = $price")
  sumPeri += price
}

var sumSide = BigInt(0)
println("====")
regions.foreach { r =>
  val c = grid.get(r.head)
  val area = r.size
  val si = sides(r)
  val price = area * si
  println(s"A region of <$c> plants with price $area * $si = $price")
  sumSide += price
}

println("total-price-of-fencing: " + sumPeri)
println("new total-price-of-fencing: " + sumSide)
