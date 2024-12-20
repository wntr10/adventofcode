import $file.^.Basic
import Basic._
import Input._
import $file.^.Grid_v3
import Grid_v3._
import scala.collection.immutable.SortedMap

//val ex = ".ex0" // 7036 45
val ex = ".ex1" // 11048 64
val inputRaw = read(s"day16$ex")
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

var grid = G(prime, maxColumns, '.')

val end = grid.findElement('E').get
val start = grid.findElement('S').get

grid = grid.updated(end.y, end.x)('.')
grid = grid.updated(start.y, start.x)('.')

println(grid)
println(s"$start -> $end")

// we use the z component for direction
def norm(p: P): P = p.copy(z = 0)

final case class E(prev: Set[P], g: BigInt, done: Boolean = false) {
  def close(): E = {
    require(!done)
    copy(done = true)
  }
}

final case class B(cost: Map[P, E], queue: SortedMap[BigInt, List[P]]) {

  def path(to: P): Set[P] = {

    def pathRec(to: P, pa: Set[P]): Set[P] = {
      val ce = cost(to)
      if (ce.prev == Set(to)) return pa + norm(to)
      var cp = pa + norm(to)
      ce.prev.foreach { pr =>
        cp ++= pathRec(pr, pa)
      }
      cp
    }

    pathRec(to, Set.empty)
  }

  def next(): (B, Option[(BigInt, P)]) = {
    //println(queue)
    //println(cost)

    var q = queue
    while (q.nonEmpty) {
      var (c, candidates) = q.head
      q = q.drop(1)
      while (candidates.nonEmpty) {
        val p = candidates.head
        candidates = candidates.drop(1)
        val pe = cost(p)
        if (!pe.done) {
          if (candidates.nonEmpty) {
            q = q.updated(c, candidates)
          }
          return (copy(queue = q, cost = cost.updated(p, pe.close())), Some((pe.g, p)))
        }
      }
    }
    (copy(queue = q), None)
  }

  def merge(prev: P, g: BigInt, p: P): B = {
    var costPrime = cost
    var queuePrime = queue

    cost.get(p) match {
      case Some(e) if e.g < g =>
      // skip
      case Some(e) if e.g == g =>
        costPrime = costPrime.updated(p, E(e.prev + prev, g))
      case _ =>
        costPrime = costPrime.updated(p, E(Set(prev), g))
        val peers = queuePrime.getOrElse(g, List.empty)
        queuePrime = queuePrime.updated(g, p :: peers)
    }
    B(cost = costPrime, queue = queuePrime)
  }
}

def nextDir(d: BigInt): BigInt = {
  (d + 1) & 3
}

def short(to: P)(from: P): Option[(Set[P], BigInt)] = {
  val es = E(Set(from), 0)
  var b = B(Map(from -> es), SortedMap(es.g -> List(from)))

  while (true) {
    b.next() match {
      case (prime, Some((cc, cp))) if norm(cp) == to =>
        b = prime
        return Some((b.path(cp), cc))
      case (prime, Some((cc, cp))) =>
        b = prime
        val p1 = cp.copy(z = nextDir(cp.z))
        b = b.merge(cp, cc + 1000, p1)
        val p2 = cp.copy(z = nextDir(p1.z))
        b = b.merge(p1, cc + 2000, p2)
        val p3 = cp.copy(z = nextDir(p2.z))
        b = b.merge(cp, cc + 1000, p3)

        val np = cp.z.toInt match {
          case 0 => cp.copy(x = cp.x + 1)
          case 1 => cp.copy(y = cp.y + 1)
          case 2 => cp.copy(x = cp.x - 1)
          case 3 => cp.copy(y = cp.y - 1)
        }

        if (grid.getOrElseZero(norm(np)) == '.') {
          b = b.merge(cp, cc + 1, np)
        }
      case (_, _) =>
        return None
    }
  }

  None
}

val p2 = short(norm(end))(start)
if (p2.isDefined) {
  grid.logWithColors(p2.get._1)
  println(s"Part One: ${p2.get._2}")
  println(s"Part Two: ${p2.get._1.size}")
}
