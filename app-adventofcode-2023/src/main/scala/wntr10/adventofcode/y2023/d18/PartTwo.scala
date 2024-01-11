package wntr10.adventofcode.y2023.d18

import wntr10.adventofcode.Input

import java.util.concurrent.TimeUnit

object PartTwo extends App {

  // TODO: Clean up and improve the Sweep-Line logic

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.parse(lines)
  }

  lazy val length: Int = input.length

  private case class Instr(dir: String, len: BigInt, color: String) {
    def fix: Instr = {
      val l = BigInt.apply(color.substring(0, 5), 16)
      val d = color.substring(5) match {
        case "0" => "R"
        case "1" => "D"
        case "2" => "L"
        case "3" => "U"
      }
      Instr(d, l, color)
    }
  }

  private def solve(input: Parser.Alpha): BigInt = {
    val fix = input.map(c => Instr(c.a, c.b, c.c)).map(_.fix).toList
    val start = System.nanoTime()
    val solution = sub(fix).sum
    println(TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start).toString + "ms")
    solution
  }

  private def sub(is: List[Instr]): List[BigInt] = {

    var x: BigInt = 0
    var y: BigInt = 0

    var xmin: BigInt = 0
    var ymin: BigInt = 0

    is.foreach {
      case Instr("U", l, _) =>
        y += l
      case Instr("D", l, _) =>
        y -= l
        ymin = ymin.min(y)
      case Instr("L", l, _) =>
        x -= l
        xmin = xmin.min(x)
      case Instr("R", l, _) =>
        x += l
      case _ => throw new RuntimeException()
    }

    x = xmin.abs
    y = ymin.abs

    var set = Set.empty[Block]

    is.foreach { in =>
      val newe: Block = in match {
        case Instr("U", l, _) =>
          val tmp = Block(x, y + 1, 1, l)
          y += l
          tmp
        case Instr("D", l, _) =>
          y -= l
          Block(x, y, 1, l)
        case Instr("L", l, _) =>
          x -= l
          Block(x, y, l, 1)
        case Instr("R", l, _) =>
          val tmp = Block(x + 1, y, l, 1)
          x += l
          tmp
        case _ => throw new RuntimeException()
      }
      set = set + newe
    }

    val ysl = set.flatMap { s =>
      List(s.y, s.maxY)
    }.toList

    val ys = ((ysl.max + 1) :: ysl).sorted

    var last = Option.empty[BigInt]
    var lastCross = List.empty[(BigInt, BigInt, String)]

    ys.map { yp =>
      var fls: BigInt = 0
      if (last.nonEmpty) {
        val my = last.get
        val bs = set.filter { b =>
          my >= b.y && my <= b.maxY
        }.toList.sortBy(_.x)

        val cross = bs.map { b =>
          if (b.y == my && b.maxY == my) {
            (b.x, b.maxX, "0")
          } else if (b.y == my) {
            (b.x, b.maxX, "1")
          } else if (b.maxY == my) {
            (b.x, b.maxX, "2")
          } else {
            (b.x, b.maxX, "3")
          }
        }

        var crossPrime = List.empty[(BigInt, BigInt, String)]
        var rest = cross
        while (rest.nonEmpty) {
          rest = rest match {

            case (s, e, "0") :: (s2, e2, "1") :: rest if s2 == e + 1 =>
              val opt = lastCross.find(c => c._1 == s || c._2 == s)
              if (opt.nonEmpty) {
                crossPrime = (s, e2, "21") :: crossPrime
              } else {
                crossPrime = (s, e2, "11") :: crossPrime
              }
              rest
            case (s, e, "1") :: (s2, e2, "0") :: rest if s2 == e + 1 =>
              val opt = lastCross.find(c => c._1 == e2 || c._2 == e2)
              if (opt.nonEmpty) {
                crossPrime = (s, e2, "12") :: crossPrime
              } else {
                crossPrime = (s, e2, "11") :: crossPrime
              }
              rest
            case (s, e, "0") :: (s2, e2, "2") :: rest if s2 == e + 1 =>
              val opt = lastCross.find(c => c._1 == s || c._2 == s)
              if (opt.nonEmpty) {
                crossPrime = (s, e2, "22") :: crossPrime
              } else {
                crossPrime = (s, e2, "12") :: crossPrime
              }
              rest
            case (s, e, "2") :: (s2, e2, "0") :: rest if s2 == e + 1 =>
              val opt = lastCross.find(c => c._1 == e2 || c._2 == e2)
              if (opt.nonEmpty) {
                crossPrime = (s, e2, "22") :: crossPrime
              } else {
                crossPrime = (s, e2, "21") :: crossPrime
              }
              rest
            case (s, e, "0") :: (s2, e2, "0") :: rest if s2 == e + 1 =>
              val optE = lastCross.find(c => c._1 == e2 || c._2 == e2).map(_ => "2").getOrElse("1")
              val optS = lastCross.find(c => c._1 == s || c._2 == s).map(_ => "2").getOrElse("1")
              val prime = optS + optE
              crossPrime = (s, e2, prime) :: crossPrime
              rest
            case (s, e, "0") :: rest =>
              val optE = lastCross.find(c => c._1 == e || c._2 == e).map(_ => "2").getOrElse("1")
              val optS = lastCross.find(c => c._1 == s || c._2 == s).map(_ => "2").getOrElse("1")
              val prime = optS + optE
              crossPrime = (s, e, prime) :: crossPrime
              rest
            case a :: rest =>
              crossPrime = a :: crossPrime
              rest
            case _ => throw new RuntimeException()
          }
        }
        crossPrime = crossPrime.reverse

        val tt1 = yp - last.get

        val cf = crossPrime.filter(e => !Set("11", "22").contains(e._3))
        require(cf.size % 2 == 0)
        require(!crossPrime.exists(c => c._3 == "0"))

        var even = true
        var start = BigInt(0)

        crossPrime.foreach { b =>
          if (even) {
            if (b._3 == "11") {
              val tt0 = (b._2 - b._1) + 1
              val tt = tt0 * tt1
              fls = fls + tt
            } else if (b._3 == "22") {
              val tt0 = (b._2 - b._1) + 1
              fls = fls + tt0
            } else if (b._3 == "21") {
              fls = fls + (b._2 - b._1)
              start = b._2
              even = false
            } else {
              start = b._1
              even = false
            }
          } else {
            if (b._3 == "22" || b._3 == "11") {
              // ignore
            } else if (b._3 == "12") {
              fls = fls + (b._2 - b._1)
              val tt0 = (b._1 - start) + 1
              val tt = tt0 * tt1
              fls = fls + tt
              even = true
            } else {
              val tt0 = (b._2 - start) + 1
              val tt = tt0 * tt1
              fls = fls + tt
              even = true
            }
          }
        }

        lastCross = crossPrime
      }
      last = Some(yp)
      fls
    }
  }

  println(solve(input))

}
