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

def run(noun: Int, verb: Int): RESULT = {
  var program = prime

  program = program.updated(1, noun)
  program = program.updated(2, verb)

  var position = 0

  while (position < prime.length) {
    val opcode = prime(position)
    opcode match {
      case 1 =>
        val a = program(position + 1)
        val b = program(position + 2)
        val c = program(position + 3)
        program = program.updated(c, program(a) + program(b))
        position = position + 4
      case 2 =>
        val a = program(position + 1)
        val b = program(position + 2)
        val c = program(position + 3)
        program = program.updated(c, program(a) * program(b))
        position = position + 4
      case 99 =>
        return program(0)
    }
  }
  0
}

Range.inclusive(0, 99).foreach { n =>
  Range.inclusive(0, 99).foreach { v =>
    val answer = run(n, v)
    if (answer == 19690720) {
      val code = 100L * n + v
      println(code)
    }
  }
}
