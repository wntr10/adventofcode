import $file.^.Basic, Basic._, Input._

val ex = ".ex0" // 11
val inputRaw = read(s"day01$ex")
val lines = splitOn("\n")(inputRaw)

type LINE = String
var prime = Vector.empty[LINE]
var countRest = 0

def visit(line: String, idx: BigInt): Unit = {
  println(s"${pad(idx)}: <$line>")
  (line, idx) match {
    case (s"$str", _) =>
      prime = prime :+ str
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

def run(): RESULT = {

  var left = List.empty[BigInt]
  var right = List.empty[BigInt]

  prime.zipWithIndex.foreach {
    case (p, _) =>
      splitOn(" ")(p) match {
        case List(a, b) =>
          left = BigInt(a) :: left
          right = BigInt(b) :: right
        case _ =>
      }
  }

  left = left.sorted
  right = right.sorted

  val r = left.zip(right).map {
    case (a, b) => (a - b).abs
  }
  r.sum

}

println(run())
