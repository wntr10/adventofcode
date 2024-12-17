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

var register = Map.empty[String, BigInt]

map("1").foreach {
  case s"Register $r: $v" =>
    register = register.updated(r, BigInt(v))
  case _ =>
}

var program = Vector.empty[Int]
map("2").foreach { l =>
  program = splitOn(",")(l.drop("Program: ".length)).map(_.toInt).toVector
}

def literal(operand: Int): BigInt = {
  operand
}

def combo(operand: Int): BigInt = {
  operand match {
    case 0 => 0
    case 1 => 1
    case 2 => 2
    case 3 => 3
    case 4 => register("A")
    case 5 => register("B")
    case 6 => register("C")
    case 7 => throw new RuntimeException()
  }
}

var o = List.empty[BigInt]

def output(value: BigInt): Unit = {
  o = value :: o
}

def run(): Unit = {

  var i = 0
  while (true) {

    if (i >= program.length) return

    val opcode = program(i)

    opcode match {
      case 0 => // adv
        val numerator = register("A")
        i += 1
        val denominator = BigInt(1) << combo(program(i)).toInt
        register = register.updated("A", numerator / denominator)
        i += 1
      case 1 => // bxl
        val tmp = register("B")
        i += 1
        val tmp2 = literal(program(i))
        register = register.updated("B", tmp ^ tmp2)
        i += 1
      case 2 => // bst
        i += 1
        val tmp2 = combo(program(i))
        register = register.updated("B", tmp2 % 8)
        i += 1
      case 3 => // jnz
        val tmp = register("A")
        if (tmp == 0) {
          i += 2
        } else {
          // not zero
          i += 1
          val tmp2 = literal(program(i))
          i = tmp2.toInt
        }
      case 4 => // bxc
        val tmp = register("B")
        i += 1
        if (i >= program.length) return
        val tmp2 = register("C")
        register = register.updated("B", tmp ^ tmp2)
        i += 1
      case 5 => // out
        i += 1
        if (i >= program.length) return
        val tmp2 = combo(program(i))
        output(tmp2 % 8)
        i += 1
      case 6 => // bdv
        val numerator = register("A")
        i += 1
        if (i >= program.length) return
        val denominator = BigInt(1) << combo(program(i)).toInt
        register = register.updated("B", numerator / denominator)
        i += 1
      case 7 => // cdv
        val numerator = register("A")
        i += 1
        if (i >= program.length) return
        val denominator = BigInt(1) << combo(program(i)).toInt
        register = register.updated("C", numerator / denominator)
        i += 1
    }
  }

}

run()

register.foreach(println)
println(o.reverse.mkString(","))
