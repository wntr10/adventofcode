package wntr10.adventofcode.y2023.d22

import com.google.common.graph.{GraphBuilder, MutableGraph}
import wntr10.adventofcode.Input

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.Random


object PartTwo extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.parse(lines)
  }

  private case class Brick(c0: List[Int], c1: List[Int], label: String = "x") {
    private val x0: Int = c0.head
    private val y0: Int = c0(1)
    private val z0: Int = c0.last

    private val x1: Int = c1.head
    private val y1: Int = c1(1)
    private val z1: Int = c1.last

    private val sx: Int = Math.min(x0, x1)
    private val sy: Int = Math.min(y0, y1)
    private val sz: Int = Math.min(z0, z1)

    require(z0 >= 1)
    require(z1 >= 1)

    private val onGround: Boolean = sz == 1

    private val h: Int = Math.abs(z1 - z0) + 1
    private val w: Int = Math.abs(x1 - x0) + 1
    private val d: Int = Math.abs(y1 - y0) + 1

    private val dd = Set(h, w, d)
    require(dd.size <= 2)

    val extend: Set[List[Int]] = {
      if (h > 1) {
        Range(0, h).map { o =>
          List(x0, y0, sz + o)
        }.toSet
      } else if (w > 1) {
        Range(0, w).map { o =>
          List(sx + o, y0, z0)
        }.toSet
      } else if (d > 1) {
        Range(0, d).map { o =>
          List(x0, sy + o, z0)
        }.toSet
      } else {
        require(dd.forall(v => v == 1))
        Set(List(x0, y0, z0))
      }
    }

    lazy val lookahead: Option[Brick] = {
      if (!onGround) {
        Some(this.copy(c0 = c0.take(2) ::: List(z0 - 1), c1 = c1.take(2) ::: List(z1 - 1)))
      } else {
        None
      }
    }

    @tailrec private final def lh(b: Brick, rest: List[Brick] = List.empty): List[Brick] = {
      val blo = b.lookahead
      if (blo.isEmpty) {
        rest
      } else {
        val bl = blo.get
        lh(bl, bl :: rest)
      }
    }

    lazy val lookaheadList: List[Brick] = lh(this)
  }

  private def solve(input: Parser.Alpha): Int = {
    var number = 0
    val arr = input.map { l =>
      var str = number.toHexString
      if (str.length < 3) {
        str = "0".repeat(3 - str.length) + str
      }
      str = "|" + str + "|"
      val brick = Brick(l.a.toList, l.b.toList, str)
      number = number + 1
      brick
    }

    val bricks = Random.shuffle(arr.toList)

    val graph = GraphBuilder.directed().build[Brick]()

    bricks.foreach { b =>
      graph.addNode(b)
    }

    bricks.foreach { b1 =>
      val list = b1.lookaheadList
      bricks.foreach { b2 =>
        if (b1 != b2) {
          val blocked = list.exists { lh =>
            lh.extend.exists(c => b2.extend.contains(c))
          }
          if (blocked) {
            graph.putEdge(b1, b2)
          }
        }
      }
    }

    var done = List.empty[Brick]
    var todo = bricks
    var stop = false
    while (!stop) {
      val start = todo.find { c =>
        val successors = graph.successors(c).asScala
        successors.isEmpty || successors.forall(s => !todo.contains(s))
      }
      if (start.isDefined) {
        var falling: Option[Brick] = start
        while (falling.isDefined) {
          val lh = falling.get.lookahead
          if (lh.isDefined) {
            val free = done.forall(d => !d.extend.exists(c => lh.get.extend.contains(c)))
            if (free) {
              falling = lh
            } else {
              done = falling.get :: done
              todo = todo.filter(b => b != start.get)
              falling = None
            }
          } else {
            done = falling.get :: done
            todo = todo.filter(b => b != start.get)
            falling = None
          }
        }
      } else {
        stop = true
      }
    }

    done = done.reverse

    disintegrate(done)
  }

  private val store = scala.collection.mutable.Map.empty[Brick, Boolean]

  private def disintegrate(bricks: List[Brick]) = {

    val graph = GraphBuilder.directed().build[Brick]()

    bricks.foreach { b =>
      graph.addNode(b)
    }

    bricks.foreach { b1 =>
      val lh = b1.lookahead
      if (lh.isDefined) {
        bricks.foreach { b2 =>
          if (b1 != b2) {
            val b1Blocked = lh.get.extend.exists(c => b2.extend.contains(c))
            if (b1Blocked) {
              graph.putEdge(b1, b2)
            }
          }
        }
      }
    }

    bricks.map { w =>
      store.clear()
      var count = -1
      bricks.foreach { c =>
        val r = block(c, w, graph)
        if (!r) {
          count += 1
        }
      }
      count
    }.sum
  }

  private def block(c: Brick, without: Brick, graph: MutableGraph[Brick]): Boolean = {
    if (store.contains(c)) {
      store(c)
    } else {
      if (c == without) {
        store.put(c, false)
        false
      } else {
        val successors = graph.successors(c).asScala
        val isBlocked = successors.exists(s => block(s, without, graph))
        if (isBlocked || successors.isEmpty) {
          store.put(c, true)
        } else {
          store.put(c, false)
        }
        isBlocked || successors.isEmpty
      }
    }
  }

  println(solve(input))
}

