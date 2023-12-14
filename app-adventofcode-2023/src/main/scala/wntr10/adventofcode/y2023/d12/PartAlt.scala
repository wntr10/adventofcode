package wntr10.adventofcode.y2023.d12

import wntr10.adventofcode.Input

import java.util.concurrent.{CompletableFuture, Executors, TimeUnit}
import java.util.function.Supplier
import scala.collection.mutable


object PartAlt extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.parse(lines)
  }

  lazy val length: Int = input.length

  private def replace(str: String): List[String] = {
    val i = str.indexOf('?')
    val arr = str.toCharArray

    arr(i) = '#'
    val a = arr.mkString("")

    arr(i) = '.'
    val bs = arr.mkString("")
    val b = normalize(bs)

    List(a, b)
  }

  private def oracle(list: List[Int]): String = {
    if (list.isEmpty) {
      "."
    } else {
      list.map { l =>
        "#".repeat(l)
      }.mkString(".", ".", ".")
    }
  }

  private def normalize(str: String): String = {
    val sb = new StringBuilder
    sb.append('.')
    var i = 0
    while (i != -1 && i < str.length) {
      val ni = str.indexOf('.', i)
      i = if (ni == -1) {
        sb.append(str.substring(i))
        sb.append('.')
        ni
      } else if (i == ni) {
        val r = str.indexOf('#', ni + 1)
        val q = str.indexOf('?', ni + 1)
        if (r == -1 || q == -1) {
          Math.max(r, q)
        } else {
          Math.min(r, q)
        }
      } else {
        sb.append(str.substring(i, ni + 1))
        val r = str.indexOf('#', ni + 1)
        val q = str.indexOf('?', ni + 1)
        if (r == -1 || q == -1) {
          Math.max(r, q)
        } else {
          Math.min(r, q)
        }
      }
    }
    sb.toString()
  }

  private def validDp(str: String, oracle: String): (Option[(String, String)], Boolean) = {
    val qi = str.indexOf('?')
    if (qi == -1) {
      (None, str == oracle)
    } else {
      val left = str.substring(0, qi)
      if (left.isEmpty) {
        (Some((str, oracle)), true)
      } else {
        if (oracle.startsWith(left)) {
          val i = left.lastIndexOf('.')
          if (i == -1) {
            (Some((str, oracle)), true)
          } else {
            (Some(str.substring(i), oracle.substring(i)), true)
          }
        } else {
          (None, false)
        }
      }
    }
  }

  private def dp(store: mutable.Map[(String, String), Long])(config: String, oracle: String): Long = {
    val key = (config, oracle)
    val cached = store.get(key)
    if (cached.isDefined) {
      cached.get
    } else {
      val count = replace(config).map { c =>
        val opt = validDp(c, oracle)
        if (!opt._2) {
          0L
        } else if (opt._1.isEmpty) {
          1L
        } else {
          val continue = opt._1.get
          dp(store)(continue._1, continue._2)
        }
      }.sum
      store.put(key, count)
      count
    }
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
          private val store = scala.collection.mutable.Map.empty[(String, String), Long]

          override def get(): Long = {
            val am = (a + "?").repeat(4) + a
            val bm = b ::: b ::: b ::: b ::: b
            val obm = oracle(bm)
            val solution = dp(store)(normalize(am), obm)
            solution
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
