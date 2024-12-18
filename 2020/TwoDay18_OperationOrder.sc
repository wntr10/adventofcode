import scala.jdk.CollectionConverters.ListHasAsScala
import $file.^.Basic
import com.google.common.base.Splitter
import fastparse._, NoWhitespace._

val ex = ".ex0" // 694173

val input = Basic.Input.read(s"day18$ex")

val lines = Splitter.on("\n").omitEmptyStrings().trimResults().splitToList(input).asScala

def number[$: P]: P[BigInt] = P(CharIn("0-9").rep(1).!.map(v => BigInt(v)))
def brackets[$: P]: P[BigInt] = P("(" ~/ precedence1 ~ ")")
def atom[$: P]: P[BigInt] = P(number | brackets)

def precedence0[$: P]: P[BigInt] = P(atom ~ (CharIn("+").! ~/ atom).rep).map(eval)
def precedence1[$: P]: P[BigInt] = P(precedence0 ~ (CharIn("*").! ~/ precedence0).rep).map(eval)

def eval(tree: (BigInt, Seq[(String, BigInt)])): BigInt = {
  val (base, ops) = tree
  var left = base
  ops.foreach {
    case ("+", right) => left += right
    case ("*", right) => left *= right
  }
  left
}

var sum = BigInt(0)

lines.map(l => l.filter(c => !c.isWhitespace)).foreach { l =>
  val Parsed.Success(result, _) = parse(l, precedence1(_))
  println(l + " becomes " + result)
  sum += result
}

println(sum)
