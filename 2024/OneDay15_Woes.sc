import $file.^.Basic
import Basic._
import Input._
import $file.^.Grid_v3, Grid_v3._

//val ex = ".ex0" // 10092
val ex = ".ex1" // 2028

val inputRaw = read(s"day15$ex")
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

val gr = map("1")
type LINE1 = Vector[Char]
var prime1 = Vector.empty[LINE1]
var maxColumns1 = 0

def visit1(line: String, idx: BigInt): Unit = {
  (line, idx) match {
    case (s"$str", _) =>
      val l = str.toVector
      prime1 = prime1 :+ l
      maxColumns1 = Math.max(maxColumns1, l.length)
    case _ =>
  }
}

gr.zipWithIndex.foreach {
  case (lines, idx) =>
    visit1(lines, idx)
}

var grid = G(prime1, maxColumns1, '.')
grid.log()

val moves = map("2").flatMap(_.toVector)

def move(o: P, d: Char): P = {
  val n = d match {
    case '<' => o.add(0, -1)
    case '>' => o.add(0, 1)
    case '^' => o.add(-1, 0)
    case 'v' => o.add(1, 0)
  }
  require(o != n)
  n
}

def swap(from: P, to: P): Unit = {
  val tmp = grid.getOrElseZero(to)
  grid = grid.updated(to.y, to.x)(grid.getOrElseZero(from))
  grid = grid.updated(from.y, from.x)(tmp)
}

def push(o: P, d: Char): Boolean = {
  val lookahead = move(o, d)
  val lc = grid.getOrElseZero(lookahead)
  lc match {
    case '.' =>
      swap(o, lookahead)
      true
    case 'O' if push(lookahead, d) =>
      swap(o, lookahead)
      true
    case _ =>
      false
  }
}

moves.foreach { m =>
  val pos = grid.findElement('@').get
  push(pos, m)
}

def gps(p: P): BigInt = {
  p.y * 100 + p.x
}

var sum = BigInt(0)
grid.findAll('O').foreach { box =>
  sum += gps(box)
}

println(sum)
