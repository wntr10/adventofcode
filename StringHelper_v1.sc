

object StringHelper {

  def count(s: String, p: String): BigInt = {
    var c = BigInt(0)
    var i = -1
    i = s.indexOf(p, i + 1)
    while (i != -1) {
      c = c + 1
      i = s.indexOf(p, i + 1)
    }
    c
  }

  def toBigInt(s: String): BigInt = {
    s.toLowerCase match {
      case "zero" => BigInt(0)
      case "one" => BigInt(1)
      case "two" => BigInt(2)
      case "three" => BigInt(3)
      case "four" => BigInt(4)
      case "five" => BigInt(5)
      case "six" => BigInt(6)
      case "seven" => BigInt(7)
      case "eight" => BigInt(8)
      case "nine" => BigInt(9)
      case n => BigInt(n)
    }
  }

  def firstMatch(s: String, set: Set[String]): Option[String] = {
    val indices = set
      .view
      .map(c => (s.indexOf(c), c))
      .filter(_._1 != -1)
      .toSet

    if (indices.isEmpty) {
      None
    } else {
      Some(indices.minBy(_._1)._2)
    }
  }

  def firstDigit(s: String): BigInt = {
    BigInt(s.find(_.isDigit).get.toString)
  }

  def fromDigits(n: BigInt*): BigInt = {
    BigInt(n.map(_.toString()).mkString)
  }

  val DIGITS = Map(
    0 -> List("0", "zero"),
    1 -> List("1", "one"),
    2 -> List("2", "two"),
    3 -> List("3", "three"),
    4 -> List("4", "four"),
    5 -> List("5", "five"),
    6 -> List("6", "six"),
    7 -> List("7", "seven"),
    8 -> List("8", "eight"),
    9 -> List("9", "nine"))

  def pad(str: String, len: Int = 4, c: Char = '_'): String = {
    val missing = (len - str.length).max(0)
    c.toString.repeat(missing) + str
  }

}
