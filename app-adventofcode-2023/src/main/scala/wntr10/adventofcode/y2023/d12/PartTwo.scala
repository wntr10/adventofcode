package wntr10.adventofcode.y2023.d12

import com.google.common.base.{CharMatcher, Splitter}
import wntr10.adventofcode.Input

import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.CollectionHasAsScala


object PartTwo extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.parse(lines)
  }

  lazy val length: Int = input.length

  private def replace(str: String): Set[String] = {
    val i = str.indexOf('?')
    val arr = str.toCharArray

    arr(i) = '#'
    val as = arr.mkString("")
    val a = normalize(as)

    arr(i) = '.'
    val bs = arr.mkString("")
    val b = normalize(bs)

    Set(a, b)
  }

  private def oracle(list: List[Int]): String = {
    list.map { l =>
      "#".repeat(l)
    }.mkString(".")
  }

  private def normalize(str: String): String = {
    Splitter.on(CharMatcher.anyOf("."))
      .omitEmptyStrings()
      .splitToList(str)
      .asScala
      .mkString(".")

  }

  private def valid(str: String, oracle: String): Option[Boolean] = {
    val f = validSub(str, oracle)
    if (f.isEmpty) {
      val f2 = validDots(str, oracle)
      if (f2.isEmpty) {
        validSub(str.reverse, oracle.reverse)
      } else {
        Some(false)
      }
    } else {
      f
    }
  }

  private def validDots(str: String, oracle: String): Option[Boolean] = {
    val min = Splitter.on('.')
      .omitEmptyStrings()
      .splitToList(str)
      .asScala
      .count(s => s.forall(f => f == '#'))

    if (min > oracle.count(c => c == '.') + 1) {
      Some(false)
    } else {
      None
    }
  }

  private def validSub(str: String, oracle: String): Option[Boolean] = {
    val qi = str.indexOf('?')
    if (qi == -1) {
      Some(str == oracle)
    } else {
      val left = str.substring(0, qi)
      if (left.isEmpty) {
        None
      } else {
        var leftStrip = left
        while (leftStrip.charAt(leftStrip.length - 1) == '.') {
          leftStrip = leftStrip.dropRight(1)
        }
        if (oracle.startsWith(leftStrip)) {
          None
        } else {
          Some(false)
        }
      }
    }
  }

  private def arrangements(str: String, q: Int, oracle: String): BigInt = {
    val n = normalize(str)
    val configs = scala.collection.mutable.Map(n -> BigInt(1))
    var r = 0
    var count = BigInt(0)
    while (r < q) {
      configs.filterInPlace { (k, v) =>
        val e = valid(k, oracle)
        if (e.isDefined && e.get) {
          count = count + ((q - r) * v)
          false
        } else if (e.isDefined) {
          false
        } else {
          true
        }
      }

      configs.toList.foreach { e =>
        configs.remove(e._1)
        if (r % 2 == 1) {
          replace(e._1).foreach { pr =>
            val i = configs.getOrElse(pr, BigInt(0))
            configs(pr) = i + e._2
          }
        } else {
          replace(e._1.reverse).foreach { pr =>
            val rpr = pr.reverse
            val i = configs.getOrElse(rpr, BigInt(0))
            configs(rpr) = i + e._2
          }
        }
      }
      r += 1
    }

    configs.filterInPlace { (k, v) =>
      val e = valid(k, oracle)

      if (e.isDefined && e.get) {
        count = count + v
        false
      } else if (e.isDefined) {
        false
      } else {
        true
      }
    }
    require(configs.isEmpty)

    count
  }

  private def solve(input: Parser.Alpha): BigInt = {
    val para = input.toList

    val start = System.nanoTime()

    val solution = para.map { elem =>
      val a = elem.a
      val b = elem.b.toList

      val am = (a + "?").repeat(4) + a
      val bm = b ::: b ::: b ::: b ::: b

      val obm = oracle(bm)

      val qam = am.count(c => c == '?')

      arrangements(am, qam, obm)
    }.sum

    println(TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start) + "ms")

    solution
  }

  println(solve(input))

}
