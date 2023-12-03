package wntr10.adventofcode


object AocInterleave {

  // from http://graphics.stanford.edu/~seander/bithacks.html)

  private val ONE = BigInt(1)

  def interleave(x: BigInt, y: BigInt): BigInt = {
    require(x >= 0 && y >= 0)
    var z = BigInt(0)
    val length = Math.max(x.bitLength, y.bitLength)

    Range(0, length).foreach { i =>
      z = z | (x & ONE << i) << i | (y & ONE << i) << (i + 1)
    }

    z
  }

  def squash(z: BigInt): (BigInt, BigInt) = {

    var x = BigInt(0)
    var y = BigInt(0)

    Range(0, z.bitLength, 2).foreach { even =>
      val shiftRight = even >> 1
      x = x | ((z & ONE << even) >> shiftRight)
      y = y | ((z & ONE << even + 1) >> shiftRight + 1)
    }

    (x, y)
  }

  // see A Practical Algorithm for Computing Neighbors in Quadtrees, Octrees, and Hyperoctrees (Yoder and Bloniarz)

  val labels: List[String] = List(
    "halt", // 0
    "R", // 1
    "L", // 2
    "D", // 3
    "U", // 4
    "RU", // 5
    "RD", // 6
    "LD", // 7
    "LU" // 8
  )

  // 2 | 3
  // 0 | 1

  //0  1  2  3
  private val next = List(
    0, 0, 0, 0, // halt
    1, 0, 3, 2, // R
    1, 0, 3, 2, // L
    2, 3, 0, 1, // D
    2, 3, 0, 1, // U
    3, 2, 1, 0, // RU
    3, 2, 1, 0, // RD
    3, 2, 1, 0, // LD
    3, 2, 1, 0).map(_.toString).toArray

  //0  1  2  3
  private val nextDir = Array[Int](
    0, 0, 0, 0, // halt
    0, 1, 0, 1, // R
    2, 0, 2, 0, // L
    3, 3, 0, 0, // D
    0, 0, 4, 4, // U
    0, 1, 4, 5, // RU
    3, 6, 0, 1, // RD
    7, 3, 2, 0, // LD
    2, 0, 8, 4) // LU


  def neighbors(key: String): List[Option[String]] = {
    var list: List[Option[String]] = List(Some(key))
    Range(1, 9).foreach { dir =>
      list = neighbor(key, dir) :: list
    }
    list.reverse
  }

  def neighbor(key: String, dir: Int): Option[String] = {
    var dkey = key
    var prime = ""
    var cd = dir
    while (dkey.nonEmpty && cd != 0) {
      val l = dkey.last.toString.toInt
      dkey = dkey.dropRight(1)
      val cdPrime = (cd << 2) + l
      cd = nextDir(cdPrime)
      prime = if (dkey.isEmpty && cd != 0) {
        ""
      } else {
        next(cdPrime) + prime
      }
    }
    if (prime.nonEmpty) {
      Some(dkey + prime)
    } else {
      None
    }
  }
}
