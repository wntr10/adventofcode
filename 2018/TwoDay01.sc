import $file.^.Basic, Basic._, Input._

val ex = ".ex1" // 14
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

def run(): Unit = {
  var sum = BigInt(0)
  var set = Set.empty[BigInt]
  while(true) {
    prime.foreach { e =>
      sum = sum + e
      if (set.contains(sum)) {
        println(sum)
        return
      }
      set = set + sum
    }
  }
}

run()
