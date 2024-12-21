import $file.^.Basic
import Basic._
import Input._
import $file.^.Grid_v3
import Grid_v3._
import $file.^.BigIntHelper_v1
import BigIntHelper_v1.BigIntHelper.vec
import $file.^.Basic
import $file.^.Path_v1, Path_v1._
import scala.collection.immutable.SortedMap

val ex = ".ex0" // 126384; 154115708116294
val inputRaw = read(s"day21$ex")
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
println(s"#lines: ${prime.size}")
println(s"#columns: $maxColumns")
println(prime)

var gridDir = G.empty[Char](vec(2, 3), '.')

val mapDir = Map(
  '^' -> P(1, 0),
  'A' -> P(2, 0),

  '<' -> P(0, 1),
  'v' -> P(1, 1),
  '>' -> P(2, 1),
)

mapDir.foreach { e =>
  gridDir = gridDir.updated(e._2.y, e._2.x)(e._1)
}

var grid = G.empty[Char](vec(4, 3), '.')

val map = Map(
  '7' -> P(0, 0),
  '8' -> P(1, 0),
  '9' -> P(2, 0),

  '4' -> P(0, 1),
  '5' -> P(1, 1),
  '6' -> P(2, 1),

  '1' -> P(0, 2),
  '2' -> P(1, 2),
  '3' -> P(2, 2),

  '0' -> P(1, 3),
  'A' -> P(2, 3),
)

map.foreach { e =>
  grid = grid.updated(e._2.y, e._2.x)(e._1)
}

grid.log()
gridDir.log()

def neighbors(grid: G[Char])(p: P): Set[P] = {
  var set = Set.empty[P]
  vec(-1, 0, 1).foreach { dy =>
    vec(-1, 0, 1).foreach { dx =>
      if (dy.abs != dx.abs) {
        val pn = p.add(dy, dx)
        grid.get(pn) match {
          case Some(_) =>
            set = set + pn
          case _ => // skip
        }
      }
    }
  }
  set
}

def short(grid: G[Char])(to: P)(from: P): Option[(Set[Vector[P]], BigInt)] = {
  val es = E(Set(from), 0)
  var b = B(Map(from -> es), SortedMap(es.g -> List(from)))

  while (true) {
    b.next() match {
      case (prime, Some((prev, cp))) if cp == to =>
        b = prime
        return Some((b.path(cp), prev))
      case (prime, Some((cc, cp))) =>
        b = prime
        neighbors(grid)(cp).foreach { np =>
          b = b.merge(cp, cc + 1, np, all = true)
        }
      case _ =>
        return None
    }
  }

  None
}

def str(pa: Vector[P]): String = {
  var s = ""
  var last = pa.head
  pa.drop(1).foreach { p =>
    ((p.x - last.x).toInt, (p.y - last.y).toInt) match {
      case (-1, 0) =>
        s += "<"
      case (1, 0) =>
        s += ">"
      case (0, -1) =>
        s += "^"
      case (0, 1) =>
        s += "v"
      case _ =>

    }
    last = p
  }
  s
}

var table = Map.empty[(Chunk, Int), BigInt]

final case class Chunk(sequence: String, vector: Vector[Chunk] = Vector.empty, offset: BigInt = 0) {
  lazy val size: BigInt = sequence.length + vector.map(_.size).sum + offset

  def split(): Chunk = {
    Chunk("", splitEndsWith("A")(sequence).map(Chunk(_)))
  }

  def grow(levels: Int): Chunk = {
    require(sequence == "")
    var offsetPrime = BigInt(0)
    this.vector.foreach { s =>
      if (table.contains((s, levels))) {
        offsetPrime += table((s, levels))
      } else {
        var prime = Set("")
        var cd = 'A'
        s.sequence.foreach { c =>
          val path = short(gridDir)(mapDir(c))(mapDir(cd)).get
          prime = prime.flatMap(k => path._1.map(pp => k + str(pp) + "A"))
          cd = c
        }
        val chunks = prime.map(u => Chunk(u).split())
        val offset = if (levels > 1) {
          chunks.map(v => v.grow(levels - 1).size).min - s.size
        } else {
          chunks.map(v => v.size).min - s.size
        }
        table = table.updated((s, levels), offset)
        offsetPrime += offset
      }
    }

    copy(offset = offsetPrime)
  }
}

def numeric(state: Char, c: Char): Set[Vector[Chunk]] = {
  val path = short(grid)(map(c))(map(state)).get
  path._1.map(pp => Vector(Chunk(str(pp) + "A")))
}

def directional(s: Chunk): Set[Vector[Chunk]] = {
  var prime = Set("")
  var state = 'A'
  s.sequence.foreach { c =>
    val path = short(gridDir)(mapDir(c))(mapDir(state)).get
    prime = prime.flatMap(k => path._1.map(pp => k + str(pp) + "A"))
    state = c
  }
  prime.map(u => Vector(Chunk(u)))
}

def combineDirectional(s: Vector[Chunk]): Set[Vector[Chunk]] = {
  var prime = Set(Vector.empty[Chunk])
  s.foreach { p =>
    prime = prime.flatMap(k => directional(p).map(g => k ++ g))
  }
  prime
}

List(1, 24).foreach { robots =>

  var complexity = BigInt(0)

  var state = 'A'

  prime.foreach { n =>

    var l = BigInt(0)

    n.foreach { c =>
      var chunkSet = Set.empty[Vector[Chunk]]
      numeric(state, c).foreach { chunks =>
        chunkSet ++= combineDirectional(chunks)
      }
      state = c
      l += chunkSet.map(_.map(_.split().grow(robots).size).sum).min
    }

    val np = BigInt(n.mkString.dropRight(1))

    complexity += l * np
  }

  println(s"complexity for $robots robots: $complexity")
}
