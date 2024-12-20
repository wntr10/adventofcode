import $ivy.`org.jgrapht:jgrapht-core:1.5.2`
import $ivy.`com.google.guava:guava:33.3.1-jre`
import $file.^.Basic
import Basic._
import Input._
import $file.^.Bag_v1
import Bag_v1._
import $file.^.Grid_v3
import Grid_v3._
import $file.^.StringHelper_v1
import StringHelper_v1._
import $file.^.BigIntHelper_v1
import BigIntHelper_v1.BigIntHelper.vec

import java.io.File

val ex = ".flip2"
val inputRaw = read(s"day0$ex")

// Scala string interpolation cannot handle escaped characters
require(!inputRaw.contains("%"))
val input = inputRaw.replace('"', '%')
val lines = splitOn("\n")(input)

//type LINE = BigInt
type LINE = Vector[Char]
//type LINE = String

var prime = Vector.empty[LINE]
var countRest = 0
var maxColumns = 0

def visit(line: String, idx: BigInt): Unit = {
  println(s"${pad(idx)}: <$line>")
  (line, idx) match {
    case (s"$str", _) =>
      //val l = str
      val l = str.toVector
      prime = prime :+ l
      maxColumns = Math.max(maxColumns, l.length)
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
println(s"#lines: ${prime.size}")
println(s"#columns: $maxColumns")
println(prime)
//println(prime.sum)

val grid = G(prime, maxColumns, '_')

def invertY(c: Char): Char = {
  c match {
    case '^' => 'v'
    case 'v' => '^'
    case _ => c
  }
}

def rotate(c: Char): Char = {
  c match {
    case '^' => '>'
    case '>' => 'v'
    case 'v' => '<'
    case '<' => '^'
    case '-' => '|'
    case '|' => '-'
    case _ => c
  }
}

def gInv(g: G[Char]): G[Char] = {
  g.invertY().mapElement(invertY)
}

def gRot(g: G[Char]): G[Char] = {
  g.invertY().swapXY().mapElement(rotate)
}

def invSwap(): Set[G[Char]] = {
  var set = Set(grid)
  var stop = false
  while (!stop) {
    var prime = Set.empty[G[Char]]
    set.foreach { s =>
      prime += gInv(s)
      prime += gRot(s)
    }
    if (set == prime) {
      stop = true
    } else {
      set ++= prime
    }
  }
  set
}


val set = invSwap()

set.foreach(_.log())
println(set.size)

val noBorder = grid.slice(R(1, 10), R(1, 10))

val a = noBorder.concatenate(noBorder, axis = 1)
val b = noBorder.concatenate(noBorder, axis = 1)
a.concatenate(b).write(new File("jigsaw-local"))
