import $file.^.Basic, Basic._, Input._

//val ex = ".ex0" // 37327623; 24
val ex = ".ex1" // 37990510; 23

val inputRaw = read(s"day22$ex")
val lines = splitOn("\n")(inputRaw)

type LINE = BigInt

var prime = Vector.empty[LINE]
var countRest = 0

def visit(line: String, idx: BigInt): Unit = {
  (line, idx) match {
    case (s"$str", _) =>
      val l = BigInt(str)
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

def mixAndPrune(secret: BigInt, b: BigInt): BigInt = {
  (secret ^ b) & 16777215 // %16777216
}

def price(secret: BigInt): Int = {
  secret.toString().last.toString.toInt
}

def generate(n: BigInt): BigInt = {
  var cn = n
  cn = mixAndPrune(cn, cn << 6) // *64
  cn = mixAndPrune(cn, cn >> 5) // /32
  mixAndPrune(cn, cn << 11) // *2048
}

def generate(n: BigInt, times: Int): Vector[BigInt] = {
  var cn = Vector(n)
  Range(0, times).foreach { _ =>
    cn = cn :+ generate(cn.last)
  }
  cn
}

def changes(n: BigInt, times: Int): Vector[Int] = {
  var cn = n
  var last = price(cn)
  var changes = Vector.empty[Int]
  Range(0, times).foreach { _ =>
    cn = generate(cn)
    val p = price(cn)
    val change = p - last
    last = p
    changes = changes :+ change
  }
  require(changes.length == times)
  changes
}

def bananas(n: BigInt, all: String, sequence: String): Int = {
  val i = all.indexOf(sequence)
  if (i == -1) {
    0
  } else {
    var p = price(n)
    all.slice(0, i + 4).foreach { s =>
      p += (s - 9 - 'a')
    }
    p
  }
}

var sumPartOne = BigInt(0)
prime.foreach { init =>
  val secret2000 = generate(init, 2000).last
  sumPartOne += secret2000
}

println(sumPartOne)
println("...")

val indices = prime.indices
val cl = prime.map(l => changes(l, 2000).map(c => (c + 9 + 'a').toChar).mkString)

var max = 0

Range.inclusive('a', 's').foreach { a =>
  Range.inclusive('a', 's').foreach { b =>
    Range.inclusive('a', 's').foreach { c =>
      Range.inclusive('a', 's').foreach { d =>
        val s = a.toChar.toString + b.toChar + c.toChar + d.toChar
        var sum = 0
        indices.foreach { i =>
          sum = sum + bananas(prime(i), cl(i), s)
        }
        max = max.max(sum)
      }
    }
  }
}

println(max)
