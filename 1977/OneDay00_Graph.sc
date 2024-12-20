import $ivy.`org.jgrapht:jgrapht-core:1.5.2`
import $ivy.`com.google.guava:guava:33.3.1-jre`
import $file.^.Basic
import Basic._
import Input._
import $file.^.Bag_v1
import Bag_v1._
import $file.^.Grid_v3
import Grid_v3._
import $file.^.StringHelper_v1
import StringHelper_v1._
import $file.^.BigIntHelper_v1
import BigIntHelper_v1._
import BigIntHelper.vec
import org.jgrapht.alg.interfaces.AStarAdmissibleHeuristic
import org.jgrapht.alg.shortestpath.{AStarShortestPath, DijkstraShortestPath}
import org.jgrapht.graph.{DefaultDirectedWeightedGraph, DefaultWeightedEdge}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.CollectionHasAsScala

//val ex = ".graph"
val ex = ".race"
val inputRaw = read(s"day0$ex")

// Scala string interpolation cannot handle escaped characters
require(!inputRaw.contains("%"))
val input = inputRaw.replace('"', '%')
val lines = splitOn("\n")(input)


type E = Char

//type LINE = BigInt
type LINE = Vector[Char]
//type LINE = String

var prime = Vector.empty[LINE]
var countRest = 0
var maxColumns = 0

def visit(line: String, idx: BigInt): Unit = {
  println(s"${pad(idx)}: <$line>")
  (line, idx) match {
    case (s"$str", _) =>
      //val l = str
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
//println(prime.sum)

var grid = G(prime, maxColumns, '#').trim()

val end = grid.findElement('E').get
val start = grid.findElement('S').get

//grid = grid.updated(end.y, end.x)('z')
//grid = grid.updated(start.y, start.x)('a')

grid = grid.updated(end.y, end.x)('.')
grid = grid.updated(start.y, start.x)('.')

grid.log(Vector(end, start))

val directedGraph = new DefaultDirectedWeightedGraph[P, DefaultWeightedEdge](classOf[DefaultWeightedEdge])

def neighbors(grid: G[E])(p: P): Unit = {
  val e = grid.get(p).get
  vec(-1, 0, 1).foreach { dy =>
    vec(-1, 0, 1).foreach { dx =>
      if (dy.abs != dx.abs) {
        val pn = p.add(dy, dx)
        grid.get(pn) match {
          case Some(nl) if nl.toInt - e.toInt <= 1 =>
            val ed = directedGraph.addEdge(p, pn)
            directedGraph.setEdgeWeight(ed, 1.0)
          case _ => // skip
        }
      }
    }
  }
}

println(grid)

grid.foreachPoint { p =>
  directedGraph.addVertex(p)
}

grid.foreachPoint { p =>
  neighbors(grid)(p)
}

/*
Range(0, grid.shape(0).toInt).foreach { y =>
  Range(0, grid.shape(1).toInt).foreach { x =>

  }
}

Range(0, grid.shape(0).toInt).foreach { y =>
  Range(0, grid.shape(1).toInt).foreach { x =>
    neighbors(P(x, y))
  }
}
*/

val h = new AStarAdmissibleHeuristic[P]() {
  override def getCostEstimate(a: P, b: P): Double = {
    a.manhattan(b).toDouble
  }
}

val dijkstra = new AStarShortestPath(directedGraph, h)
//val dijkstra = new DijkstraShortestPath(directedGraph)

Option(dijkstra.getPath(start, end)).foreach { path =>
  println(s"Path(length=${path.getLength},weight=${path.getWeight})")
  val pa = path.getVertexList.asScala.toVector
  grid.logWithColors(pa.toSet)
}

var length = Option.empty[BigInt]
var weight = Option.empty[Double]


var shortMap = Map.empty[P, BigInt]

//grid.findAll('a').foreach { a =>
grid.findAll('.').foreach { a =>
  Option(dijkstra.getPath(a, end)).foreach { path =>
    println(s"Path(length=${path.getLength},weight=${path.getWeight})")
    //val pa = path.getVertexList.asScala.toVector
    length = length.orElse(Some(path.getLength))
    length = length.map(l => l.min(path.getLength))

    weight = weight.orElse(Some(path.getWeight))
    weight = weight.map(w => Math.min(w, path.getWeight))

    shortMap = shortMap.updated(a, path.getWeight.toInt)
  }
}

println(s"MinimumPath(length=$length,weight=$weight)")

val max = shortMap.maxByOption(_._2)
max.foreach { m =>
  shortMap = shortMap.map(e => e._1 -> (m._2 - e._2))
}


@tailrec
def bfs(filter: P => Boolean)(nxt: Vector[P], done: Map[P, BigInt], bound: Option[BigInt] = None): Map[P, BigInt] = {
  //println(s"bfs(nxt=${nxt.take(5)}...,done=${done.take(5)}...,bound=$bound)")
  if (nxt.isEmpty) return done
  val p = nxt.head
  require(done.contains(p))
  var nxtPrime = nxt.drop(1)
  val step = done(p)

  var donePrime = done
  if (bound.isEmpty || step + 1 < bound.get) {
    vec(-1, 0, 1).foreach { dy =>
      vec(-1, 0, 1).foreach { dx =>
        if (dy.abs != dx.abs) {
          val n = p.add(dy, dx)
          if (!donePrime.contains(n) && filter(n)) {
            nxtPrime = nxtPrime :+ n
            donePrime = donePrime.updated(n, step + 1)
          }
        }
      }
    }
  }

  bfs(filter)(nxtPrime, donePrime, bound)
}


def dfs(filter: P => Boolean)(p: P, done: Set[P] = Set.empty): Set[P] = {

  var current = done + p

  vec(-1, 0, 1).foreach { dy =>
    vec(-1, 0, 1).foreach { dx =>
      if (dy.abs != dx.abs) {
        val n = p.add(dy, dx)
        if (!current.contains(n) && filter(n)) {
          current = dfs(filter)(n, current)
        }
      }
    }
  }

  current
}


val flooded = bfs(grid.contains)(Vector(start), Map(start -> 0))
val circle = bfs(grid.isInBounds)(Vector(end), Map(end -> 0), Some(10))

grid.logWithColors(flooded.keySet, circle.keySet)

println(flooded)
println(shortMap)

require(flooded == shortMap)
