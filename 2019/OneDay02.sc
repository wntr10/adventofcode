import $file.^.Basic, Basic._, Input._

val ex = ""
val inputRaw = read(s"day02$ex")
val lines = split(",", inputRaw)

type LINE = Int
var prime = Vector.empty[LINE]
var countRest = 0

def visit(line: String, idx: BigInt): Unit = {
  println(s"${pad(idx)}: <$line>")
  (line, idx) match {
    case (s"$str", _) =>
      prime = prime :+ str.toInt
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

type RESULT = Int

def run(): RESULT = {

  // 1202 program alarm
  prime = prime.updated(1, 12)
  prime = prime.updated(2, 2)

  var position = 0

  while (position < prime.length) {
    val opcode = prime(position)
    opcode match {
      case 1 =>
        val a = prime(position + 1)
        val b = prime(position + 2)
        val c = prime(position + 3)
        prime = prime.updated(c, prime(a) + prime(b))
        position = position + 4
      case 2 =>
        val a = prime(position + 1)
        val b = prime(position + 2)
        val c = prime(position + 3)
        prime = prime.updated(c, prime(a) * prime(b))
        position = position + 4
      case 99 =>
        return prime(0)
    }
  }
  0
}

println(run())
