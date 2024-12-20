import $ivy.`org.jgrapht:jgrapht-core:1.5.2`
import $file.^.Basic
import Basic._
import Input._
import $file.^.Grid_v3
import Grid_v3._
import $file.^.BigIntHelper_v1
import BigIntHelper_v1.BigIntHelper.vec
import org.jgrapht.alg.interfaces.AStarAdmissibleHeuristic
import org.jgrapht.alg.shortestpath.AStarShortestPath
import org.jgrapht.graph.{DefaultDirectedWeightedGraph, DefaultWeightedEdge}

val ex = ".ex0" // Some(22); Some(P(6,1,0,0))

//val m = 71
//val t = 1024

val m = 7
val t = 12

val inputRaw = read(s"day18$ex")
val lines = splitOn("\n")(inputRaw)

type LINE = Vector[Char]

var listOfBytes = Vector.empty[P]

def visit(line: String, idx: BigInt): Unit = {
  (line, idx) match {
    case (s"$x,$y", _) =>
      listOfBytes = listOfBytes :+ P(x.toInt, y.toInt)
    case _ =>
  }
}

lines.zipWithIndex.foreach {
  case (lines, idx) =>
    visit(lines, idx)
}

def test(grid: G[Char]): Option[Int] = {

  val start = P()
  val end = P(m - 1, m - 1)

  val directedGraph = new DefaultDirectedWeightedGraph[P, DefaultWeightedEdge](classOf[DefaultWeightedEdge])

  def neighbors(x: BigInt, y: BigInt): Unit = {
    vec(-1, 0, 1).foreach { dy =>
      vec(-1, 0, 1).foreach { dx =>
        if (dy.abs != dx.abs) {
          val np = P(x + dx, y + dy)
          grid.getOrElseZero(np) match {
            case '.' if grid.isInBounds(np) =>
              val ed = directedGraph.addEdge(P(x, y), np)
              directedGraph.setEdgeWeight(ed, 1.0)
            case _ =>
          }
        }
      }
    }
  }

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

  def manhattan(a: P, b: P): BigInt = {
    (b.y - a.y).abs + (b.x - a.x).abs
  }

  val h = new AStarAdmissibleHeuristic[P]() {
    override def getCostEstimate(v: P, v1: P): Double = {
      manhattan(v, v1).toDouble
    }
  }

  val dijkstraAlg = new AStarShortestPath(directedGraph, h)
  Option(dijkstraAlg.getPath(start, end)).map(_.getLength)
}

{
  var grid = G.empty(vec(m, m), '.')
  listOfBytes.take(t).foreach { b =>
    grid = grid.updated(b.y, b.x)('#')
  }
  println(test(grid))
}

def find(): Option[P] = {
  var grid = G.empty(vec(m, m), '.')
  listOfBytes.foreach { b =>
    grid = grid.updated(b.y, b.x)('#')
    if (test(grid).isEmpty) {
      return Some(b)
    }
  }
  None
}

println(find())
