import $file.^.Basic, Basic._, Input._

val ex = ".ex0" // 1928
val inputRaw = read(s"day09$ex")
val lines = splitOn("\n")(inputRaw)

type LINE = Vector[Char]

var prime = Vector.empty[LINE]
var countRest = 0
var maxColumns = 0

def visit(line: String, idx: BigInt): Unit = {
  (line, idx) match {
    case (s"$str", _) =>
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

case class BLOCK(idOpt: Option[Int], length: Int) {

  def isFile: Boolean = idOpt.isDefined

  def isSpace: Boolean = !isFile

  def id: Int = idOpt.get

  override def toString: String = {
    if (isFile) {
      id.toString.repeat(length)
    } else {
      ".".repeat(length)
    }
  }
}

var file = true
var i = -1

val s = prime.head.map { c =>
  if (file) {
    file = false
    i = i + 1
    BLOCK(Some(i), c.toString.toInt)
  } else {
    file = true
    BLOCK(None, c.toString.toInt)
  }
}

var norm = s.flatMap { b =>
  var span = Vector.empty[BLOCK]
  Range(0, b.length).foreach { _ =>
    span = span :+ b.copy(length = 1)
  }
  span
}

def swap(a: Int, b: Int): Unit = {
  norm = norm.updated(b, norm(a)).updated(a, norm(b))
}

def compact(): Unit = {
  while (true) {
    val lf = norm.lastIndexWhere(_.isFile)
    val fs = norm.indexWhere(_.isSpace)
    if (fs < lf) {
      swap(lf, fs)
    } else {
      return
    }
  }
}

compact()

println(norm.mkString)

var checksum = BigInt(0)
norm.zipWithIndex.map {
  case (b, i) if b.isFile =>
    checksum = checksum + (b.id * i)
  case _ =>
  // skip
}

println(s"checksum=$checksum")
