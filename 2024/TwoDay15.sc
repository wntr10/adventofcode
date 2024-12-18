import $file.^.Basic
import Basic._
import Input._
import $file.^.Grid_v3, Grid_v3._
import $file.^.BigIntHelper_v1, BigIntHelper_v1.BigIntHelper.vec

val ex = ".ex0" // 9021

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

var gridOrig = G(prime1, maxColumns1, '.')

var grid = G.empty(vec(prime1.size, maxColumns1 << 1), '.')

gridOrig.delegate.foreach {
  case (p, '#') =>
    grid = grid.updated(p.y, p.x << 1)('#')
    grid = grid.updated(p.y, (p.x << 1) + 1)('#')
  case (p, 'O') =>
    grid = grid.updated(p.y, p.x << 1)('[')
    grid = grid.updated(p.y, (p.x << 1) + 1)(']')
  case (p, '@') =>
    grid = grid.updated(p.y, p.x << 1)('@')
  case _ =>
}

grid.log()

val moves = map("2").flatMap(_.toVector)

def move(o: P, d: Char): P = {
  d match {
    case '<' => o.add(0, -1)
    case '>' => o.add(0, 1)
    case '^' => o.add(-1, 0)
    case 'v' => o.add(1, 0)
  }
}

def swap(from: P, to: P): Unit = {
  val tmp = grid.get(to)
  grid = grid.updated(to.y, to.x)(grid.get(from))
  grid = grid.updated(from.y, from.x)(tmp)
}

def pushLookahead(o: P, d: Char): Boolean = {
  val lookahead = move(o, d)
  val lc = grid.get(lookahead)
  (lc, d) match {
    case ('.', _) =>
      true
    case ('#', _) =>
      false
    case (']', '<') =>
      pushLookahead(lookahead.add(0, -1), d)
    case ('[', '>') =>
      pushLookahead(lookahead.add(0, 1), d)
    case ('[', _) =>
      pushLookahead(lookahead.add(0, 1), d) && pushLookahead(lookahead, d)
    case (']', _) =>
      pushLookahead(lookahead.add(0, -1), d) && pushLookahead(lookahead, d)
  }
}

def push(o: P, d: Char): Boolean = {
  val lookahead = move(o, d)
  val lc = grid.get(lookahead)
  (lc, d) match {
    case ('#', _) =>
      false
    case ('.', _) =>
      swap(o, lookahead)
      true
    case (']', '<') =>
      if (push(lookahead.add(0, -1), d)) {
        push(lookahead, d)
        swap(o, lookahead)
        true
      } else {
        false
      }
    case ('[', '>') =>
      if (push(lookahead.add(0, 1), d)) {
        push(lookahead, d)
        swap(o, lookahead)
        true
      } else {
        false
      }
    case ('[', _) =>
      if (pushLookahead(lookahead, d) && pushLookahead(lookahead.add(0, 1), d)) {
        push(lookahead, d)
        push(lookahead.copy(x = lookahead.x + 1), d)
        swap(o, lookahead)
        true
      } else {
        false
      }
    case (']', _) =>
      if (pushLookahead(lookahead, d) && pushLookahead(lookahead.add(0, -1), d)) {
        push(lookahead, d)
        push(lookahead.add(0, -1), d)
        swap(o, lookahead)
        true
      } else {
        false
      }
  }
}

moves.foreach { m =>
  val pos = grid.find('@').get
  push(pos, m)
}

grid.log()

def gps(p: P): BigInt = {
  p.y * 100 + p.x
}

var sum = BigInt(0)
grid.findAll('[').foreach { box =>
  sum += gps(box)
}

println(sum)
