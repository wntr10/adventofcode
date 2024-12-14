case class BigDirection(dir: BigInt) {

  //  6   7   0
  //    \ | /
  //  5 --+-- 1
  //    / | \
  //  4   3   2

  val horizontal: Boolean = dir == 1 || dir == 5

  val vertical: Boolean = dir == 7 || dir == 3

  val plus: Boolean = (dir & 1) == 1

  def turn180(): BigDirection = this.copy((dir + 4) % 8)

  def turnClockwise90(): BigDirection = this.copy((dir + 2) % 8)

  def turnAntiClockwise90(): BigDirection = turnClockwise90().turn180()

  override def toString: String = {
    val str = dir.toInt match {
      case 1 => "RR"
      case 2 => "RD"
      case 3 => "DD"
      case 4 => "LD"
      case 5 => "LL"
      case 6 => "LU"
      case 7 => "UU"
      case 0 => "RU"
    }
    s"($dir)$str"
  }
}

object BigDirection {
  def apply(d: BigInt) = new BigDirection(d)

  def apply(d: String) = d.toLowerCase match {
    case "up" => new BigDirection(7)
    case "right" => new BigDirection(1)
    case "down" => new BigDirection(3)
    case "left" => new BigDirection(5)
  }

  def yx(d: BigInt): BigDirection = d.toInt match {
    case 0 => BigDirection(6)
    case 1 => BigDirection(7)
    case 2 => BigDirection(0)
    case 3 => BigDirection(5)
    case 4 => BigDirection(1)
    case 5 => BigDirection(4)
    case 6 => BigDirection(3)
    case 7 => BigDirection(2)
  }

}
