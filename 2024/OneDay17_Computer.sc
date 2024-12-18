import $file.^.Basic, Basic._, Input._

val ex = ".ex0" // 4,6,3,5,6,3,5,2,1,0

val inputRaw = read(s"day17$ex")
val parts = split("\n\n", inputRaw)

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

type REG = Map[String, BigInt]

var register: REG = Map.empty

map("1").foreach {
  case s"Register $r: $v" =>
    register = register.updated(r, BigInt(v))
  case _ =>
}

var program = Vector.empty[Int]
map("2").foreach { l =>
  program = splitOn(",")(l.drop("Program: ".length)).map(_.toInt).toVector
}


final class Program(init: REG) {

  private var register = init

  val A = "A"
  val B = "B"
  val C = "C"

  private def literal(op: Int): BigInt = {
    op
  }

  private def combo(operand: Int): BigInt = {
    operand match {
      case 0 => 0
      case 1 => 1
      case 2 => 2
      case 3 => 3
      case 4 => register(A)
      case 5 => register(B)
      case 6 => register(C)
      case 7 => throw new RuntimeException()
    }
  }

  var o = List.empty[BigInt]

  private def output(value: BigInt): Unit = {
    o = value :: o
  }

  def run(): REG = {

    var i = 0
    while (i < program.length) {

      val opcode = program(i)
      val op = program(i + 1)

      opcode match {
        case 0 => // adv
          val numerator = register(A)
          val denominator = BigInt(1) << combo(op).toInt
          register = register.updated(A, numerator / denominator)
          i += 2
        case 1 => // bxl
          val tmp = register(B)
          val tmp2 = literal(op)
          register = register.updated(B, tmp ^ tmp2)
          i += 2
        case 2 => // bst
          val tmp2 = combo(op)
          register = register.updated(B, tmp2 & 7)
          i += 2
        case 3 if register(A) == 0 => // jnz skip
          i += 2
        case 3 => // jnz
          i = literal(op).toInt
        case 4 => // bxc
          register = register.updated(B, register(B) ^ register(C))
          i += 2
        case 5 => // out
          output(combo(op) & 7)
          i += 2
        case 6 => // bdv
          val numerator = register(A)
          val denominator = BigInt(1) << combo(op).toInt
          register = register.updated(B, numerator / denominator)
          i += 2
        case 7 => // cdv
          val numerator = register(A)
          val denominator = BigInt(1) << combo(op).toInt
          register = register.updated(C, numerator / denominator)
          i += 2
      }
    }
    register
  }
}

val p = new Program(register)
val registerPrime = p.run()

registerPrime.foreach(println)
println(p.o.reverse.mkString(","))
