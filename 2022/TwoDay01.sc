import $file.^.Basic, Basic._, Input._

val ex = ".ex0" // 45000

val inputRaw = read(s"day01$ex")
val parts = split("\n\n", inputRaw)

type LINE = BigInt
var map = Map.empty[String, Vector[LINE]]
var countRest = 0

def visit(line: String, idx: BigInt, prime: Vector[LINE]): Vector[LINE] = {
  println(s"${pad(idx)}: <$line>")
  (line, idx) match {
    case (s"$str", _) =>
      prime :+ BigInt(str)
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
    println(s"--- $id ---")
    var prime = Vector.empty[LINE]

    lines.drop(0).zipWithIndex.foreach {
      case (lines, idx) =>
        prime = visit(lines, idx, prime)
    }

    map = map.updated(id, prime)
}

require(countRest == 0)

type RESULT_PART = BigInt

def runPart(part: Vector[LINE]): RESULT_PART = {
  var r: RESULT_PART = 0
  part.zipWithIndex.foreach {
    case (p, _) =>
      r = r + p
  }
  r
}

type RESULT = Vector[RESULT_PART]

def run(): RESULT = {
  var r: RESULT = Vector.empty
  map.foreach {
    case (_, part) =>
      r = r :+ runPart(part)
  }
  r
}

println(run().sorted.reverse.take(3).sum)
