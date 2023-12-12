package wntr10.adventofcode.y2023.d12

import com.google.common.base.{CharMatcher, Splitter}
import wntr10.adventofcode.Input

import java.util.concurrent.{CompletableFuture, Executors, TimeUnit}
import java.util.function.Supplier
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
      validSub(str.reverse, oracle.reverse)
    } else {
      f
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

  private def filter(map: scala.collection.mutable.Map[String, Long], oracle: String): Long = {
    var count = 0L
    map.filterInPlace { (k, v) =>
      val e = valid(k, oracle)
      if (e.isDefined && e.get) {
        count = count + v
        false
      } else {
        e.isEmpty
      }
    }
    count
  }

  private def arrangements(str: String, q: Int, oracle: String): Long = {
    val n = normalize(str)
    val configs = scala.collection.mutable.Map(n -> 1L)
    var r = 0
    var count = 0L
    while (r < q) {
      count = count + filter(configs, oracle)

      configs.toList.foreach { e =>
        configs.remove(e._1)
        if (r % 2 == 1) {
          replace(e._1).foreach { pr =>
            val i = configs.getOrElse(pr, 0L)
            configs(pr) = i + e._2
          }
        } else {
          replace(e._1.reverse).foreach { pr =>
            val rpr = pr.reverse
            val i = configs.getOrElse(rpr, 0L)
            configs(rpr) = i + e._2
          }
        }
      }
      r += 1
    }

    count = count + filter(configs, oracle)

    require(configs.isEmpty)

    count
  }

  private def solve(input: Parser.Alpha): Long = {
    val para = input.toList

    val service = Executors.newFixedThreadPool(32)
    val start = System.nanoTime()

    val solution = try {

      val supplier = para.map { elem =>
        new Supplier[Long]() {
          private val a = elem.a
          private val b = elem.b.toList

          override def get(): Long = {
            val am = (a + "?").repeat(4) + a
            val bm = b ::: b ::: b ::: b ::: b

            val obm = oracle(bm)

            val qam = am.count(c => c == '?')

            arrangements(am, qam, obm)
          }
        }
      }

      val future = supplier.map { s =>
        CompletableFuture.supplyAsync(s, service)
      }

      future.map(_.get()).sum

    } finally {
      service.shutdownNow()
    }

    println(TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start).toString + "ms")

    solution
  }

  println(solve(input))

}
