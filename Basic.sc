import com.google.common.base.{Charsets, Splitter}
import com.google.common.io.Files

import java.io.File
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.CollectionHasAsScala

object Input {
  def read(name: String): String = {
    val file = new File(name)
    Files.asCharSource(file, Charsets.UTF_8).read() + "\n"
  }

  def split(c: String, input: String) = {
    Splitter
      .on(c)
      .omitEmptyStrings()
      .trimResults()
      .splitToList(input)
      .asScala
      .toList

  }
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

def toBigInt(s: String): BigInt = {
  s match {
    case "zero" => BigInt(0)
    case "one" => BigInt(1)
    case "two" => BigInt(2)
    case "three" => BigInt(3)
    case "four" => BigInt(4)
    case "five" => BigInt(5)
    case "six" => BigInt(6)
    case "seven" => BigInt(7)
    case "eight" => BigInt(8)
    case "nine" => BigInt(9)
    case n => BigInt(n)
  }
}

def firstMatch(s: String, set: Set[String]): Option[String] = {
  val indices = set
    .view
    .map(c => (s.indexOf(c), c))
    .filter(_._1 != -1)
    .toSet

  if (indices.isEmpty) {
    None
  } else {
    Some(indices.minBy(_._1)._2)
  }
}

def firstDigit(s: String): BigInt = {
  BigInt(s.find(_.isDigit).get.toString)
}

def fromDigits(n: BigInt*): BigInt = {
  BigInt(n.map(_.toString()).mkString)
}

val DIGITS = Map(
  0 -> List("0", "zero"),
  1 -> List("1", "one"),
  2 -> List("2", "two"),
  3 -> List("3", "three"),
  4 -> List("4", "four"),
  5 -> List("5", "five"),
  6 -> List("6", "six"),
  7 -> List("7", "seven"),
  8 -> List("8", "eight"),
  9 -> List("9", "nine"))

def pad(v: BigInt, len: BigInt = 4): String = {
  val str = v.toString()
  val missing = (len - str.length).max(0)
  "0".repeat(missing.toInt) + str
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

def lehmer(a: Vector[Int]): Vector[Int] = {
  //val s = a.sorted
  //s.zipWithIndex
  a
}

def factoradic(a: Vector[Int]): BigInt = {
  var sum = BigInt(0)
  a.reverse.zipWithIndex.foreach {
    case (d, i) =>
      sum = sum + (d * factorial(i))
  }
  sum
}

def baseFac(a: BigInt): Vector[Int] = {

  //var sum = BigInt(0)
  //a.reverse.zipWithIndex.foreach {
  //case (d, i) =>
  //sum = sum + (d * factorial(i))
  //}
  //sum
  Vector.empty
}

def swap[T](a: Vector[T], b: Int, c: Int): Vector[T] = {
  a.updated(c, a(b)).updated(b, a(c))
}

def even(k: Int): Boolean = {
  (k & 1) == 0
}

def even(k: BigInt): Boolean = {
  (k & 1) == 0
}

def permutations[T](k: Int, a: Vector[T], visit: Vector[T] => Unit): Unit = {
  if (k == 1) {
    visit(a)
  } else {
    permutations(k - 1, a, visit)
    val isEven = even(k)
    var ca = a
    Range(0, k - 1).foreach { i =>
      ca = if (isEven) {
        swap(ca, i, k - 1)
      } else {
        swap(ca, 0, k - 1)
      }
      permutations(k - 1, ca, visit)
    }
  }
}


def factorial(n: BigInt): BigInt = {
  fac(n, 1)
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
  private val side = sqrtN(size)

  def xy(idx: BigInt): (BigInt, BigInt) = {
    val x = idx % side
    val y = idx / side
    (x, y)
  }

  def right(i: BigInt): Option[BigInt] = {
    val r = (i % side) + 1
    if (r < side) {
      Some(i + 1)
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
