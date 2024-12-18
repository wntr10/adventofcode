import $ivy.`org.jgrapht:jgrapht-core:1.5.2`
import $ivy.`com.google.guava:guava:33.3.1-jre`
import $file.^.Basic
import Basic._
import Input._
import $file.^.Bag_v1
import Bag_v1._
import $file.^.Grid_v3, Grid_v3._
import $file.^.StringHelper_v1
import StringHelper_v1._
import $file.^.BigIntHelper_v1
import BigIntHelper_v1.BigIntHelper.vec
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph.{DefaultDirectedWeightedGraph, DefaultWeightedEdge}

import scala.jdk.CollectionConverters.CollectionHasAsScala

val ex = ".graph"
val inputRaw = read(s"day0$ex")

// Scala string interpolation cannot handle escaped characters
require(!inputRaw.contains("%"))
val input = inputRaw.replace('"', '%')
val lines = splitOn("\n")(input)

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

var grid = G(prime, maxColumns, '.')

val end = grid.find('E').get
val start = grid.find('S').get

//grid.log(Vector(P(0, 0), P(1, 1), P(2, 2)))

grid = grid.updated(end.y, end.x)('z')
grid = grid.updated(start.y, start.x)('a')

println(grid.find('E'))
println(grid.find('S'))

grid.log(Vector(end, start))

val directedGraph = new DefaultDirectedWeightedGraph[P, DefaultWeightedEdge](classOf[DefaultWeightedEdge])

def neighbors(x: BigInt, y: BigInt): Unit = {
  val level = grid(y, x)
  vec(-1, 0, 1).foreach { dy =>
    vec(-1, 0, 1).foreach { dx =>
      if (dy.abs != dx.abs) {
        val np = P(x + dx, y + dy)
        val levelNeighbor = grid.get(np)
        //println(s"from $x,$y ($level) -> $nx,$ny ($levelNeighbor)")

        levelNeighbor match {
          case '.' => // skip
            //println(s"from $x,$y ($level) -> $nx,$ny ($levelNeighbor) X.")
          case nl if nl.toInt - level.toInt <= 1 =>
            val ed = directedGraph.addEdge(P(x, y), np)
            directedGraph.setEdgeWeight(ed, 1.0)
            //println(s"from $x,$y ($level) -> $nx,$ny ($levelNeighbor)")
          case _ =>
           // println(s"from $x,$y ($level) -> $nx,$ny ($levelNeighbor) X/")
        }
      }
    }
  }
}

println(grid)

Range(0, grid.shape(0).toInt).foreach { y =>
  Range(0, grid.shape(1).toInt).foreach { x =>
    directedGraph.addVertex(P(x, y))
  }
}

Range(0, grid.shape(0).toInt).foreach { y =>
  Range(0, grid.shape(1).toInt).foreach { x =>
    neighbors(x, y)
  }
}

val dijkstraAlg = new DijkstraShortestPath(directedGraph)
val p = dijkstraAlg.getPath(start, end)
if (p != null) {
  println(p.getLength)
  val pa = p.getVertexList.asScala.toVector
  grid.log(pa)
}

val d = grid.findAll('a').map { a =>
  val p = dijkstraAlg.getPath(a, end)
  if (p != null) {
    Some(p.getLength)
  } else {
    None
  }
}.filter(_.isDefined).map(_.get).min

println(d)
