import $file.^.Basic, Basic._, Input._

val ex = ".ex1" // 9
val inputRaw = read(s"day02$ex")
val lines = split("\n", inputRaw)

type LINE = List[BigInt]
var prime = Vector.empty[LINE]
var countRest = 0

def visit(line: String, idx: BigInt): Unit = {
  println(s"${pad(idx)}: <$line>")
  (line, idx) match {
    case (s"$str", _) =>
      val l = split("\t", str).map(n => BigInt(n))
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

type RESULT = BigInt

def run(): RESULT = {
  var r: RESULT = 0
  prime.zipWithIndex.foreach {
    case (p, _) =>
      p.zipWithIndex.foreach {
        case (a, ai) =>
          p.zipWithIndex.foreach {
            case (b, bi) =>
              if (ai != bi) {
                if (a % b == 0) {
                  r = r + (a / b)
                }
              }
          }
      }
  }
  r
}

println(run())
