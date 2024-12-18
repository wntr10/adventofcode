import $file.^.Basic, Basic._, Input._
import $file.^.StringHelper_v1, StringHelper_v1._

val ex = ".ex0" // (3749,11387)
val inputRaw = read(s"day07$ex")
val lines = splitOn("\n")(inputRaw)

case class EQ(test: BigInt, ops: Vector[BigInt])

type LINE = EQ
var prime = Vector.empty[LINE]
var countRest = 0

def visit(line: String, idx: BigInt): Unit = {
  (line, idx) match {
    case (s"$t:$str", _) =>
      val o = splitOn(" ")(str).toVector.map(BigInt(_))
      prime = prime :+ EQ(BigInt(t), o)
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

type RESULT = BigInt

def eval(eq: EQ)(v: Vector[Int]): Boolean = {
  var r = eq.ops.head
  eq.ops.drop(1).zip(v).foreach {
    case (o, 0) =>
      r += o
    case (o, 1) =>
      r *= o
    case (o, 2) =>
      r = StringHelper.fromDigits(r, o)
  }
  eq.test == r
}

def combinations(visit: Vector[Int] => Boolean,
                 open: Vector[BigInt],
                 done: Vector[Int]): (Boolean, Boolean) = {

  if (open.isEmpty) {
    val r = visit(done)
    (r, r)
  } else {
    val (mul1, mul2) = combinations(visit, open.drop(1), done :+ 0)
    val (add1, add2) = combinations(visit, open.drop(1), done :+ 1)
    val (_, concat2) = combinations(visit, open.drop(1), done :+ 2)
    (mul1 || add1, mul2 || add2 || concat2)
  }
}

def run(): (RESULT, RESULT) = {
  var one: RESULT = 0
  var two: RESULT = 0

  prime.zipWithIndex.foreach {
    case (p, _) =>
      val v = eval(p)(_)
      val (e1, e2) = combinations(v, p.ops.drop(1), Vector.empty)
      if (e1) {
        one += p.test
      }
      if (e2) {
        two += p.test
      }
  }
  (one, two)
}

println(run())
