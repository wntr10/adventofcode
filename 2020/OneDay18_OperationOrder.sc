import scala.jdk.CollectionConverters.ListHasAsScala
import $file.^.Basic
import com.google.common.base.Splitter
import fastparse._, NoWhitespace._

val ex = ".ex0" // 26457

val input = Basic.Input.read(s"day18$ex")

val lines = Splitter.on("\n").omitEmptyStrings().trimResults().splitToList(input).asScala

def number[$: P]: P[BigInt] = P(CharIn("0-9").rep(1).!.map(v => BigInt(v)))

// ~/ is a cut; i.e. a marker that states that you cannot backtrack past this point

def brackets[$: P]: P[BigInt] = P("(" ~/ expr ~ ")")
def atom[$: P]: P[BigInt] = P(number | brackets)
def expr[$: P]: P[BigInt] = P(atom ~ (CharIn("+*").! ~/ atom).rep).map(eval)

def eval(tree: (BigInt, Seq[(String, BigInt)])): BigInt = {
  val (base, ops) = tree
  var left = base
  ops.foreach {
    case ("+", right) => left = left + right
    case ("*", right) => left = left * right
  }
  left
}

var sum = BigInt(0)

lines.map(l => l.filter(c => !c.isWhitespace)).foreach { l =>
  val Parsed.Success(result, _) = parse(l, expr(_))
  println(l + " becomes " + result)
  sum += result
}

println(sum)
