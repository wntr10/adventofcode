import $file.^.Basic, Basic._, Input._

val ex = ".ex0" // 4
val inputRaw = read(s"day02$ex")
val lines = splitOn("\n")(inputRaw)

type LINE = String
var prime = Vector.empty[LINE]
var countRest = 0

def visit(line: String, idx: BigInt): Unit = {
  println(s"${pad(idx)}: <$line>")
  (line, idx) match {
    case (s"$str", _) =>
      prime = prime :+ str
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

type RESULT = BigInt

def findSafeReport(report: List[BigInt]): Boolean = {
  if (!nonSafe(report)) {
    return true
  }

  report.indices.foreach { i =>
    val (left, right) = report.splitAt(i)
    if (!nonSafe(left ++ right.drop(1))) {
      return true
    }
  }
  false
}

def nonSafe(report: List[BigInt]): Boolean = {
  var last = Option.empty[BigInt]
  var increasing = Option.empty[Boolean]
  var nok = false
  report.foreach { level =>
    last.foreach { l =>
      val delta = level - l
      nok = nok || delta.abs < 1 || delta.abs > 3
      if (increasing.isDefined) {
        if (increasing.get) {
          nok = nok || delta < 0
        } else {
          nok = nok || delta > 0
        }
      } else {
        increasing = Some(delta > 0)
      }
    }
    last = Some(level)
  }
  nok
}

def run(): RESULT = {
  var r: RESULT = 0
  prime.zipWithIndex.foreach {
    case (p, _) =>
      val report = splitOn(" ")(p).map(e => BigInt(e))
      if (findSafeReport(report)) {
        r += 1
      }
  }
  r
}

println(run())
