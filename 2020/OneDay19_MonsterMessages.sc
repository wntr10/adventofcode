import $ivy.`org.jgrapht:jgrapht-core:1.5.2`
import $ivy.`com.google.guava:guava:33.3.1-jre`
import $file.^.Basic
import Basic._, Input._

val ex = ".ex1" // 2

val inputRaw = read(s"day19$ex")
val input = inputRaw.replace('"', '%')

val lines = split("\n", input)

trait RULE {
  def matches(str: String): (Boolean, Set[String])

  def full(str: String): Boolean = {
    val (m, r) = matches(str)
    m && r.exists(s => s.isEmpty)
  }
}

var rules = Map.empty[Int, RULE]

case class ONE(single: String) extends RULE {
  def matches(str: String): (Boolean, Set[String]) = {
    if (str.startsWith(single)) {
      (true, Set(str.drop(single.length)))
    } else {
      (false, Set(str))
    }
  }
}

case class SEQ(seq: List[Int]) extends RULE {
  def matches(str: String): (Boolean, Set[String]) = {
    var s = Set(str)
    seq.foreach { idx =>
      val rule = rules(idx)
      var ps = Set.empty[String]
      s.foreach { ts =>
        val (rm, rs) = rule.matches(ts)
        if (rm) {
          ps = ps ++ rs
        }
      }
      if (ps.isEmpty) {
        return (false, Set(str))
      }
      s = ps
    }
    (true, s)
  }
}

case class AT_LEAST_ONE(alt: Set[List[Int]]) extends RULE {
  def matches(str: String): (Boolean, Set[String]) = {
    val m = alt.map(a => SEQ(a).matches(str)).filter(_._1)
    if (m.isEmpty) {
      (false, Set(str))
    } else {
      (true, m.flatMap(_._2))
    }
  }
}

var messages = List.empty[String]

var rest = 0
lines.foreach {
  case s"$str" if str.head.isLetter =>
    messages = str :: messages
  case s"$a: %$b%" =>
    rules = rules.updated(a.toInt, ONE(b))
  case s"$a: $b | $c" =>
    val bb = split(" ", b).map(_.toInt)
    val cc = split(" ", c).map(_.toInt)
    rules = rules.updated(a.toInt, AT_LEAST_ONE(Set(bb, cc)))
  case s"$a: $b" =>
    val bb = split(" ", b).map(_.toInt)
    rules = rules.updated(a.toInt, SEQ(bb))
  case r =>
    rest = rest + 1
    println(s"REST <$r>")
}

require(rest == 0)

val zero = rules(0)

var count = 0
messages.foreach { msg =>
  if (zero.full(msg)) {
    count = count + 1
  }
}

println(count)
