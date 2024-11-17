package wntr10.adventofcode.y2023.d21

import wntr10.adventofcode.AocDirection.{DOWN, LEFT, RIGHT, UP}
import wntr10.adventofcode._
import wntr10.adventofcode.y2023.d21.Tiles.{EMPTY, Wrapper}

object PartTwo extends App {

  private val store = scala.collection.mutable.Map.empty[(Set[AocKey], Set[AocKey], Set[AocKey], Set[AocKey], Set[AocKey]), Set[AocKey]]
  private val tag = scala.collection.mutable.Map.empty[Set[AocKey], Int]

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  private lazy val grid = AnotherGrid.of(input, s => AocStringValue(s))

  val dirs = Set(LEFT, RIGHT, UP, DOWN)

  private case class Ring(current: Tiles)

  private case class RingOut(current: Tiles, reuse: Boolean)

  private def fnc(grid: AnotherGrid)(ring: Ring): RingOut = {

    val group = ring.current.group

    val extend = group.keySet.flatMap { k =>
      Set(k, (k._1 - 1, k._2), (k._1 + 1, k._2), (k._1, k._2 - 1), (k._1, k._2 + 1))
    }

    var pr: Tiles = Tiles.empty

    var reuse = true

    extend.foreach { g =>
      val mod = g
      val center = group.getOrElse(mod, EMPTY)
      val left = group.getOrElse((mod._1 - 1, mod._2), EMPTY)
      val right = group.getOrElse((mod._1 + 1, mod._2), EMPTY)
      val top = group.getOrElse((mod._1, mod._2 + 1), EMPTY)
      val bottom = group.getOrElse((mod._1, mod._2 - 1), EMPTY)

      val memk = (center, left, right, top, bottom)

      val mem = store.get(memk)

      val re = {
        if (mem.isDefined) {
          mem.get
        } else {
          reuse = false
          var centerPrime = EMPTY

          center.foreach { k =>
            dirs.foreach { d =>
              grid.neighbor(k, d.dir, v => v.str == "." || v.str == "S").foreach { n =>
                val vir = virtual(n.key)
                if (vir._1 == (0, 0)) {
                  val point = AocPoint(vir._2._1, vir._2._2)
                  if (!center.contains(point)) {
                    centerPrime = centerPrime + point
                  }
                }
              }
            }
          }

          left.foreach { k =>
            dirs.foreach { d =>
              grid.neighbor(k, d.dir, v => v.str == "." || v.str == "S").foreach { n =>
                val vir = virtual(n.key)
                if (vir._1 == (1, 0)) {
                  val point = AocPoint(vir._2._1, vir._2._2)
                  if (!center.contains(point)) {
                    centerPrime = centerPrime + point
                  }
                }
              }
            }
          }

          right.foreach { k =>
            dirs.foreach { d =>
              grid.neighbor(k, d.dir, v => v.str == "." || v.str == "S").foreach { n =>
                val vir = virtual(n.key)
                if (vir._1 == (-1, 0)) {
                  val point = AocPoint(vir._2._1, vir._2._2)
                  if (!center.contains(point)) {
                    centerPrime = centerPrime + point
                  }
                }
              }
            }
          }

          top.foreach { k =>
            dirs.foreach { d =>
              grid.neighbor(k, d.dir, v => v.str == "." || v.str == "S").foreach { n =>
                val vir = virtual(n.key)
                if (vir._1 == (0, -1)) {
                  val point = AocPoint(vir._2._1, vir._2._2)
                  if (!center.contains(point)) {
                    centerPrime = centerPrime + point
                  }
                }
              }
            }
          }

          bottom.foreach { k =>
            dirs.foreach { d =>
              grid.neighbor(k, d.dir, v => v.str == "." || v.str == "S").foreach { n =>
                val vir = virtual(n.key)
                if (vir._1 == (0, 1)) {
                  val point = AocPoint(vir._2._1, vir._2._2)
                  if (!center.contains(point)) {
                    centerPrime = centerPrime + point
                  }
                }
              }
            }
          }

          store.put(memk, centerPrime)
          centerPrime
        }
      }

      pr = pr.update(mod._1, mod._2, re)
    }

    RingOut(pr, reuse)
  }

  private val virtual: AocKey => ((BigInt, BigInt), (BigInt, BigInt)) = grid.virtual()

  private def solve(): Int = {

    val icurrent = grid.nodes.filter(n => n.value.str == "S").map(n => Wrapper(virtual(n.key)))

    var current: Tiles = Tiles.from(icurrent)

    var steps = 0

    var reuse = false
    var reuse2 = false

    while (!(reuse && reuse2)) {

      println("EVEN steps=" + steps + " / tiles=" + current.tiles + " / plots=" + current.size + " / " + store.size)

      val ring = Ring(current)

      val ringOut = fnc(grid)(ring)
      current = ringOut.current
      reuse = ringOut.reuse

      steps += 1

      println("ODD  steps=" + steps + " / tiles=" + current.tiles + " / plots=" + current.size + " / " + store.size)

      val ring2 = Ring(current)

      val ringOut2 = fnc(grid)(ring2)
      current = ringOut2.current
      reuse2 = ringOut2.reuse

      steps += 1

    }

    println("FINAL steps=" + steps + " / tiles=" + current.tiles + " / plots=" + current.size + " / " + store.size)

    var ts = Set.empty[Set[AocKey]]

    store.foreach { e =>
      ts = ts + e._1._1
      ts = ts + e._2
    }

    println("--")

    ts.foreach { e =>
      val n = number(e)
      println(s"$n -> ${e.size},")
    }

    println("--")

    var done = Set.empty[Set[AocKey]]
    store.keySet.foreach { k =>
      val l = List(k._1, k._2, k._3, k._4, k._5)
      val ks = l.map(s => number(s)).mkString(", ")
      val v = store(k)
      val vs = number(v)
      val effect = k._1 != v

      if (!done.contains(k._1)) {
        val same = store.filter(f => f._1._1 == k._1).values.toSet.size == 1

        if (same && effect) {
          done = done + k._1
        }

        val empt = v == Tiles.EMPTY
        if (!empt) {
          val str = if (effect && same) "('A', " else if (effect) "('E', " else "''"
          println(str + ks + ") -> " + vs + ",")
        }
      }
    }

    val vs = store.values.toSet
    val start = store.filter(e => !vs.contains(e._1._1))
    println("---")
    println(start)
    println(number(start.head._1._1))

    0
  }

  def number(set: Set[AocKey]): Int = {
    val opt = tag.get(set)
    if (opt.isDefined) {
      opt.get
    } else {
      val nn = if (tag.nonEmpty) {
        tag.values.max + 1
      } else {
        1
      }
      tag.put(set, nn)
      nn
    }
  }

  println(solve())

}
