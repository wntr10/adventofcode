import $file.^.Basic, Basic._, Input._

val ex = ".ex0" // 143, 123

val inputRaw = read(s"day05$ex")
val parts = splitOn("\n\n")(inputRaw)

type LINE = String
var map = Map.empty[String, Vector[LINE]]
var countRest = 0

def visit(line: String, idx: BigInt, prime: Vector[LINE]): Vector[LINE] = {
  (line, idx) match {
    case (s"$str", _) =>
      prime :+ str
    case (l, i) =>
      countRest = countRest + 1
      println(s"REST ${pad(i)}: <$l>")
      prime
  }
}

parts.zipWithIndex.foreach {
  case (part, pi) =>
    val lines = split("\n", part)

    val id = (pi + 1).toString
    var prime = Vector.empty[LINE]

    lines.drop(0).zipWithIndex.foreach {
      case (lines, idx) =>
        prime = visit(lines, idx, prime)
    }

    map = map.updated(id, prime)
}

require(countRest == 0)

case class RULE(a: Int, b: Int)

var rules = Map.empty[Int, Set[RULE]]

map("1").foreach { p =>
  splitOn("|")(p).map(_.toInt) match {
    case a :: b :: Nil =>
      val c = rules.getOrElse(a, Set.empty)
      rules = rules.updated(a, c + RULE(a, b))
    case _ =>
  }
}

val comp = new Ordering[Int] {

  private def lessThan(x: Int, y: Int): Boolean = {
    rules
      .getOrElse(x, Set.empty)
      .exists(r => r.b == y)
  }

  override def compare(x: Int, y: Int): Int = {
    if (lessThan(x, y)) {
      -1
    } else if (lessThan(y, x)) {
      1
    } else {
      0
    }
  }
}

var ok = BigInt(0)
var nok = BigInt(0)

map("2").foreach { p =>
  val original = splitOn(",")(p).toVector.map(_.toInt)
  val m = original.size >> 1
  // sort must be stable
  val sorted = original.sorted(comp)
  if (original == sorted) {
    ok = ok + original(m)
  } else {
    nok = nok + sorted(m)
  }
}

println(ok)
println(nok)
