import $file.^.Basic, Basic._, Input._

val ex = ".ex0" // 241861950
val inputRaw = read(s"day01$ex")
val lines = split("\n", inputRaw)

type LINE = BigInt
var prime = Vector.empty[LINE]
var countRest = 0

def visit(line: String, idx: BigInt): Unit = {
  println(s"${pad(idx)}: <$line>")
  (line, idx) match {
    case (s"$str", _) =>
      prime = prime :+ BigInt(str)
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
  prime.zipWithIndex.foreach {
    case (a, ai) =>
      prime.zipWithIndex.foreach {
        case (b, bi) =>
          prime.zipWithIndex.foreach {
            case (c, ci) =>
              if (Set(ai, bi, ci).size == 3) {
                if (a + b + c == 2020) {
                  return a * b * c
                }
              }
          }
      }
  }
  0
}

println(run())
