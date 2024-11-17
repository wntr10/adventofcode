package wntr10.adventofcode.y2023.d10

import com.google.common.graph.{ElementOrder, GraphBuilder, MutableGraph}
import wntr10.adventofcode._

import scala.jdk.CollectionConverters.CollectionHasAsScala


object PartOne extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  lazy val length: Int = input.length

  private def connect(s: AocNode, t: Option[AocNode], graph: MutableGraph[AocNode]): Unit = {
    t.foreach { o =>
      graph.putEdge(s, o)
      graph.putEdge(o, s)
    }
  }

  private def canConnect(dir: Int): AocValue => Boolean = {
    dir match {
      case 1 =>
        v => Set("-", "J", "7").contains(v.str)
      case 2 =>
        v => Set("-", "F", "L").contains(v.str)
      case 3 =>
        v => Set("L", "|", "J").contains(v.str)
      case 4 =>
        v => Set("F", "|", "7").contains(v.str)
    }
  }

  private def linkFun(n: AocNode, grid: AocGrid, graph: MutableGraph[AocNode])(dirA: Int, dirB: Int): Unit = {
    val a = grid.neighbor(n.key, dirA, canConnect(dirA))
    val b = grid.neighbor(n.key, dirB, canConnect(dirB))
    connect(n, a, graph)
    connect(n, b, graph)
  }

  private def solve(input: Parser.Alpha): Int = {
    val grid = AocGrid.of(input, s => AocStringValue(s))
    val subGrid = grid.filter(e => e.value.str != ".")

    val graph = GraphBuilder
      .directed()
      .nodeOrder(ElementOrder.sorted(AocNodeComparator))
      .build[AocNode]()
    ()

    subGrid.foreach { n =>
      graph.addNode(n)
      val link: (Int, Int) => Unit = linkFun(n, subGrid, graph)
      n.value.str match {
        case "|" =>
          link(4, 3)
        case "-" =>
          link(2, 1)
        case "L" =>
          link(4, 1)
        case "J" =>
          link(4, 2)
        case "7" =>
          link(3, 2)
        case "F" =>
          link(3, 1)
        case "S" =>
          val t = List(
            subGrid.neighbor(n.key, 1, canConnect(1)),
            subGrid.neighbor(n.key, 2, canConnect(2)),
            subGrid.neighbor(n.key, 3, canConnect(3)),
            subGrid.neighbor(n.key, 4, canConnect(4))
          ).filter(_.isDefined).map(_.get)

          connect(n, Some(t.head), graph)
          connect(n, Some(t.last), graph)
        case _ =>
      }
    }

    var current = graph.nodes().asScala.filter(n => n.value.str == "S").toSet
    var done = current
    var steps = 0
    while (current.nonEmpty) {
      current = current.flatMap { n =>
        graph.successors(n).asScala.toSet
      }
      current = current.diff(done)
      done = done ++ current
      steps += 1
    }
    steps - 1
  }

  println(solve(input))
}
