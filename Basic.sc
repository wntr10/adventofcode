import com.google.common.base.{Charsets, Splitter}
import com.google.common.collect.Iterables
import com.google.common.io.Files

import java.io.File
import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

object Input {
  def read(name: String): String = {
    val file = new File(name)
    Files.asCharSource(file, Charsets.UTF_8).read() + "\n"
  }

  def splitOn(on: String)(str: String) = split(on, str)

  def split(on: String, str: String) = {
    Splitter
      .on(on)
      .omitEmptyStrings()
      .trimResults()
      .splitToList(str)
      .asScala
      .toList

  }
}

def cycle(list: List[Int]): Iterator[Int] = {
  Iterables.cycle(list.asJava).iterator().asScala
}

def distance(a: String, b: String): BigInt = {
  require(a.length == b.length)
  var d = BigInt(0)
  a.zip(b).foreach {
    case (ac, bc) =>
      d = d + Math.abs(ac.toInt - bc.toInt)
  }
  d
}

@tailrec
def radix(n: BigInt, mix: List[BigInt], digits: List[BigInt] = List.empty): List[BigInt] = {
  if (mix.isEmpty) {
    return digits.reverse
  }
  val base = mix.head
  val d = n / base
  val r = n - (d * base)
  radix(r, mix.drop(1), d :: digits)
}

def factoradic(a: Vector[Int]): BigInt = {
  var sum = BigInt(0)
  a.reverse.zipWithIndex.foreach {
    case (d, i) =>
      sum = sum + (d * factorial(i))
  }
  sum
}

def even(k: BigInt): Boolean = {
  (k & 1) == 0
}

def factorial(n: BigInt): BigInt = {
  fac(n, 1)
}

def pad(v: BigInt, len: BigInt = 4): String = {
  val str = v.toString()
  val missing = (len - str.length).max(0)
  "0".repeat(missing.toInt) + str
}

@tailrec
def fac(n: BigInt, acc: BigInt): BigInt = {
  if (n == 0) {
    return acc
  }
  fac(n - 1, acc * n)
}

def sqrtN(in: BigInt): BigInt = {
  val TWO = BigInt(2)
  var c = 0
  var n0 = TWO.pow(in.bitLength >> 1)
  var np = in

  do {
    n0 = (n0 + (in / n0)) >> 1
    c = np.compareTo(n0)
    np = n0
  } while (c != 0)

  n0
}

class LinearSquare(size: BigInt) {
  val side = sqrtN(size)

  def corners: Set[BigInt] = Set(0, side - 1, size - side, size - 1)

  def vistLast[T](v: Vector[T], x: (BigInt, BigInt) => Unit, y: (BigInt, BigInt) => Unit): Unit = {

    require(v.size <= size)
    val p = BigInt(v.size - 1)

    left(p).foreach { l =>
      x(l, p)
    }

    right(p).foreach { r =>
      if (r < v.size) {
        x(p, r)
      }
    }

    up(p).foreach { u =>
      y(u, p)
    }

    down(p).foreach { d =>
      if (d < v.size) {
        y(p, d)
      }
    }
  }

  def xy(idx: BigInt): (BigInt, BigInt) = {
    val x = idx % side
    val y = idx / side
    (x, y)
  }

  def left(i: BigInt): Option[BigInt] = {
    val l = (i % side) - 1
    if (l >= 0) {
      Some(i - 1)
    } else {
      None
    }
  }

  def right(i: BigInt): Option[BigInt] = {
    val r = (i % side) + 1
    if (r < side) {
      Some(i + 1)
    } else {
      None
    }
  }

  def up(i: BigInt): Option[BigInt] = {
    val d = i - side
    if (d >= 0) {
      Some(d)
    } else {
      None
    }
  }

  def down(i: BigInt): Option[BigInt] = {
    val d = i + side
    if (d < size) {
      Some(d)
    } else {
      None
    }
  }
}
