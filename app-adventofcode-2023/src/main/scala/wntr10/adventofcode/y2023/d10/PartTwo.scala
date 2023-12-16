package wntr10.adventofcode.y2023.d10

import com.google.common.graph.{ElementOrder, GraphBuilder, MutableGraph}
import wntr10.adventofcode._

import scala.jdk.CollectionConverters.CollectionHasAsScala


object PartTwo extends App {

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

    val c = graph.nodes().asScala.filter { n => n.value.str == "S" }.head

    var done = Set(c)
    var current = Set(c)

    while (current.nonEmpty) {
      val cp = current.flatMap { c =>
        graph.successors(c).asScala.toSet
      }
      current = cp.diff(done)
      done = done ++ current
    }

    val loopGrid = grid.map(e => if (!done.exists(p => p.key == e._1)) (e._1, AocStringValue(".")) else e)
    val check = loopGrid.nodes.diff(done)

    val out = check.filter { n =>
      findAWayOut(n, loopGrid)
    }

    check.size - out.size
  }

  private def next(state: (Int, Int, Int, Int), dir: Int, n: AocNode): Option[(AocNode, (Int, Int, Int, Int))] = {
    val e = (state._1, state._2, state._3, state._4, dir, n.value.str) match {

      case (a, _, c, _, 2, "F") =>
        (n, (a, a, a, c))
      case (a, _, c, _, 2, "L") =>
        (n, (c, a, c, c))
      case (a, _, c, _, 2, "J") =>
        (n, (0, a, c, c))
      case (a, _, c, _, 2, "7") =>
        (n, (a, a, 0, c))
      case (a, _, c, _, 2, "-") =>
        (n, (a, a, c, c))
      case (a, _, c, _, 2, "|") =>
        (n, (0, a, 0, c))

      case (_, b, _, d, 1, "F") =>
        (n, (b, b, d, 0))
      case (_, b, _, d, 1, "L") =>
        (n, (b, 0, d, d))
      case (_, b, _, d, 1, "J") =>
        (n, (b, d, d, d))
      case (_, b, _, d, 1, "|") =>
        (n, (b, 0, d, 0))
      case (_, b, _, d, 1, "7") =>
        (n, (b, b, d, b))
      case (_, b, _, d, 1, "-") =>
        (n, (b, b, d, d))

      case (a, b, _, _, 4, "F") =>
        (n, (a, a, a, b))
      case (a, b, _, _, 4, "L") =>
        (n, (a, 0, a, b))
      case (a, b, _, _, 4, "J") =>
        (n, (0, b, a, b))
      case (a, b, _, _, 4, "7") =>
        (n, (b, b, a, b))
      case (a, b, _, _, 4, "-") =>
        (n, (0, 0, a, b))
      case (a, b, _, _, 4, "|") =>
        (n, (a, b, a, b))

      case (_, _, c, d, 3, "F") =>
        (n, (c, d, c, 0))
      case (_, _, c, d, 3, "L") =>
        (n, (c, d, c, c))
      case (_, _, c, d, 3, "J") =>
        (n, (c, d, d, d))
      case (_, _, c, d, 3, "7") =>
        (n, (c, d, 0, d))
      case (_, _, c, d, 3, "-") =>
        (n, (c, d, 0, 0))
      case (_, _, c, d, 3, "|") =>
        (n, (c, d, c, d))

      case (a, b, _, _, 4, ".") =>
        (n, (a, b, a, b))
      case (a, _, c, _, 2, ".") =>
        (n, (a, a, c, c))
      case (_, b, _, d, 1, ".") =>
        (n, (b, b, d, d))
      case (_, _, c, d, 3, ".") =>
        (n, (c, d, c, d))
      case (_, _, _, _, _, _) =>
        (n, (0, 0, 0, 0))
    }
    if (e._2 == (0, 0, 0, 0)) {
      None
    } else {
      Some(e)
    }
  }


  private def findAWayOut(n: AocNode, grid: AocGrid): Boolean = {
    val dirs = Set(1, 2, 3, 4)
    var done = Set(n)
    var current = Set((n, (1, 1, 1, 1)))
    while (current.nonEmpty) {
      current = current.flatMap { c =>
        val set = dirs.map { dir =>
          (grid.neighbor(c._1.key, dir, _ => true), dir)
        }
        if (set.exists { s =>
          (c._2._1, c._2._2, c._2._3, c._2._4, s._2, s._1) match {
            case (a, _, c, _, 2, None) if a == 1 || c == 1 =>
              true
            case (_, b, _, d, 1, None) if b == 1 || d == 1 =>
              true
            case (_, _, c, d, 3, None) if c == 1 || d == 1 =>
              true
            case (a, b, _, _, 4, None) if a == 1 || b == 1 =>
              true
            case _ =>
              false
          }
        }) {
          return true
        }
        set.filter(_._1.isDefined).map(e => (e._1.get, e._2)).flatMap { s =>
          next(c._2, s._2, s._1)
        }
      }
      current = current.filter(c => !done.contains(c._1))
      current.foreach { c =>
        done = done + c._1
      }
    }

    false
  }

  println(solve(input))

}
