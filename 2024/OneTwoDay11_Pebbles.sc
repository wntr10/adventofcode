import $file.^.Basic, Basic._, Input._

val input = "125 17"  // 55312, 65601038650482

var table = Map.empty[(BigInt, Int), BigInt]

def size(i: Int)(nn: BigInt): BigInt = {
  if (table.contains((nn, i))) {
    return table((nn, i))
  }
  val s = (nn, i, nn.toString()) match {
    case (_, 0, _) =>
      BigInt(1)
    case (n, _, _) if n == 0 =>
      size(i - 1)(BigInt(1))
    case (_, _, digits) if (digits.length & 1) == 0 =>
      val (left, right) = digits.splitAt(digits.length >> 1)
      size(i - 1)(BigInt(left)) + size(i - 1)(BigInt(right))
    case (n, _, _) =>
      size(i - 1)(n * 2024)
  }
  table = table.updated((nn, i), s)
  s
}

val stones = splitOn(" ")(input).map(e => BigInt(e)).toVector

{
  val st = size(25) _
  println(stones.map(st).sum)
}

{
  val st = size(75) _
  println(stones.map(st).sum)
}
