import $ivy.`org.jgrapht:jgrapht-core:1.5.2`
import $file.BigIntHelper_v1
import BigIntHelper_v1.BigIntHelper._
import $file.Grid_v3
import Grid_v3._
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph.{DefaultDirectedWeightedGraph, DefaultWeightedEdge}

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.Random


def manhattan(a: P, b: P): BigInt = {
  (b.y - a.y).abs + (b.x - a.x).abs
}

final case class E(prev: P, g: BigInt, f: BigInt, done: Boolean = false) {
  def close(): E = {
    require(!done)
    copy(done = true)
  }
}

final case class B(cost: Map[P, E], queue: SortedMap[BigInt, List[P]]) {

  def path(to: P): Vector[P] = {

    @tailrec
    def pathRec(to: P, pa: Vector[P]): Vector[P] = {
      val ce = cost(to)
      if (ce.prev == to) return to +: pa
      pathRec(ce.prev, to +: pa)
    }

    pathRec(to, Vector.empty)
  }

  def next(): (B, Option[(BigInt, P)]) = {
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

  def merge(prev: P, g: BigInt, f: BigInt, p: P): B = {
    var costPrime = cost
    var queuePrime = queue

    cost.get(p) match {
      case Some(e) if e.g <= g =>
      // skip
      case _ =>
        costPrime = costPrime.updated(p, E(prev, g, f))
        val peers = queuePrime.getOrElse(f, List.empty)
        queuePrime = queuePrime.updated(f, p :: peers)
    }
    B(cost = costPrime, queue = queuePrime)
  }

}


def short(o: G[Char])(to: P)(from: P): Option[(Vector[P], BigInt)] = {
  val es = E(from, 0, 0)
  var b = B(Map(from -> es), SortedMap(es.f -> List(from)))

  while (true) {
    b.next() match {
      case (prime, Some((cc, cp))) if cp == to =>
        b = prime
        return Some((b.path(to), cc))
      case (prime, Some((cc, cp))) =>
        b = prime
        vec(-1, 0, 1).foreach { dy =>
          vec(-1, 0, 1).foreach { dx =>
            val np = cp.add(dy, dx)
            if (dy.abs != dx.abs && o.isInBounds(np)) {
              o(np.y, np.x) match {
                case '.' | '0' => // skip
                case nw =>
                  val g = cc + nw.toString.toInt
                  val f = g + manhattan(np, to)
                  b = b.merge(cp, g, f, np)
              }
            }
          }
        }
      case (_, None) =>
        return None
    }
  }

  None
}


{
  var o = G.empty(vec(50, 200), '.')
  o.log()

  val rnd = Random

  for (y <- 0 until 50) {
    for (x <- 0 until 200) {
      val w = rnd.nextInt(10)
      o = o.updated(y, x)(w.toString.charAt(0))
    }
  }
  o.log()

  val directedGraph = new DefaultDirectedWeightedGraph[P, DefaultWeightedEdge](classOf[DefaultWeightedEdge])

  def neighbors(x: BigInt, y: BigInt): Unit = {
    vec(-1, 0, 1).foreach { dy =>
      vec(-1, 0, 1).foreach { dx =>
        val np = P(x + dx, y + dy)
        if (dy.abs != dx.abs && o.isInBounds(np)) {
          o(np.y, np.x) match {
            case '.' | '0' => // skip
            case w =>
              val ed = directedGraph.addEdge(P(x, y), np)
              directedGraph.setEdgeWeight(ed, w.toString.toDouble)
            case _ =>
          }
        }
      }
    }
  }

  for (y <- 0 until 50) {
    for (x <- 0 until 200) {
      directedGraph.addVertex(P(x, y))
    }
  }

  for (y <- 0 until 50) {
    for (x <- 0 until 200) {
      neighbors(x, y)
    }
  }

  val dijkstraAlg = new DijkstraShortestPath(directedGraph)
  val p = dijkstraAlg.getPath(P(0, 0), P(199, 49))
  if (p != null) {
    println("LEN " + p.getLength)
    println("COST " + p.getWeight)
    val pa = p.getVertexList.asScala.toVector
    o.log(pa)
  }
  println("MY")
  val p2 = short(o)(P(199, 49))(P(0, 0))
  if (p2.isDefined) {
    println("LEN1 " + p.getLength)
    println("COST1 " + p.getWeight)
    println("LEN2 " + p2.get._1.size)
    println("COST2 " + p2.get._2)
    val pa = p2.get._1
    o.log(pa)
  }

}
