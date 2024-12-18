import $file.^.Basic, Basic._, Input._

val ex = ".ex0" // 3, 3
val inputRaw = read(s"day11$ex")
val line = splitOn("\n")(inputRaw).head

type LINE = Vector[String]

var countRest = 0

def visit(line: String): LINE = {
  line match {
    case s"$str" =>
      splitOn(",")(str).toVector
    case l =>
      countRest = countRest + 1
      println(s"REST <$l>")
      throw new RuntimeException()
  }
}

val prime = visit(line)

//  (+s)
//    \
//     \  n  /
//      +---+
//  nw /  q  \ ne
//   -+       +------ (+q)
//  sw \s   r/ se
//      +---+
//     /  s  \
//    /
//  (+r)


var q = 0
var s = 0
var r = 0

var max = 0

def distance(a : (Int, Int, Int), b : (Int, Int, Int)) = {
  (Math.abs(a._1 - b._1) + Math.abs(a._3 - b._3) + Math.abs(a._2 - b._2)) / 2
}

prime.foreach { p =>
  println(p)
  p match {
    case "n" =>
      //
      s = s + 1
      r = r - 1
    case "ne" =>
      q = q + 1
      //
      r = r - 1
    case "se" =>
      q = q + 1
      s = s - 1
    //
    case "s" =>
      //
      s = s - 1
      r = r + 1
    case "sw" =>
      q = q - 1
      //
      r = r + 1
    case "nw" =>
      q = q - 1
      s = s + 1
    //
  }
  require(q + s + r == 0)
  max = Math.max(max, distance((0, 0, 0), (q, s, r)))
}

println(distance((0, 0, 0), (q, s, r)))
println(max)

