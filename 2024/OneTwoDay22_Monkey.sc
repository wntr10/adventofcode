import $file.^.Basic
import Basic._
import Input._

import scala.annotation.tailrec

//val ex = ".ex0" // 37327623; 24
val ex = ".ex1" // 37990510; 23

val inputRaw = read(s"day22$ex")
val lines = splitOn("\n")(inputRaw)

type LINE = BigInt

var prime = Vector.empty[LINE]
var countRest = 0

def visit(line: String, idx: BigInt): Unit = {
  (line, idx) match {
    case (s"$str", _) =>
      val l = BigInt(str)
      prime = prime :+ l
    case (l, i) =>
      countRest = countRest + 1
      println(s"REST ${pad(i)}: <$l>")
  }
}

lines.zipWithIndex.foreach {
  case (lines, idx) =>
    visit(lines, idx)
}

require(countRest == 0)

def mixAndPrune(s: BigInt, b: BigInt) = (s ^ b) % 16777216
def price(s: BigInt) = (s % 10).toInt

def generate(_n: BigInt): BigInt = {
  var n = _n
  n = mixAndPrune(n, n * 64)
  n = mixAndPrune(n, n / 32)
  mixAndPrune(n, n * 2048)
}

@tailrec
def generate(times: Int)(n: BigInt): BigInt = {
  times match {
    case 0 => n
    case _ => generate(times - 1)(generate(n))
  }
}

// replaced my solution with the one of steffenmaus

var map = Map.empty[Vector[Int], Vector[Int]]

def changes(times: Int)(_n: BigInt): Unit = {
  var n = _n
  var last = price(n)
  var set = Set.empty[Vector[Int]]
  var w = Vector.empty[Int]
  Range(0, times).foreach { _ =>
    n = generate(n)
    val p = price(n)
    w = w :+ (p - last)
    last = p
    w = w.drop(w.size - 4)
    if (w.size == 4 && !set.contains(w)) {
      set += w
      val me = map.getOrElse(w, Vector.empty)
      map = map.updated(w, me :+ p)
    }
  }
}

println(prime.map(generate(2000)).sum)
prime.foreach(changes(2000))
println(map.map(_._2.sum).max)
