package wntr10.adventofcode.y2023.d25

import org.jgrapht.alg.StoerWagnerMinimumCut
import org.jgrapht.graph.{DefaultWeightedEdge, SimpleWeightedGraph}
import wntr10.adventofcode.Input

object PartOne extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.parse(lines)
  }

  private def solve(input: Parser.Alpha): Long = {

    var graph = Set.empty[Set[String]]
    input.foreach { g =>
      g.b.foreach { k =>
        graph = graph + Set(g.a, k)
      }
    }

    val components = graph.flatten
    val builder = SimpleWeightedGraph.createBuilder[String, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
    components.foreach(c => builder.addVertex(c))
    graph.foreach(e => builder.addEdge(e.head, e.last, 1.0))
    val minCut = new StoerWagnerMinimumCut(builder.build())
    val set = minCut.minCut()

    (components.size - set.size()) * set.size()
  }


  println(solve(input))

}
