import $file.Basic
import Basic._
import $file.Bag_v1
import Bag_v1._
import $file.BigIntHelper_v1, BigIntHelper_v1.BigIntHelper.vec

val sub = List(0, 1, 2)
val list = sub ++ sub ++ sub ++ sub
list.zipWithIndex.foreach {
  case (e, i) =>
    require(e == i % sub.length)
}

{
  val str = "012x345"
  println(str.splitAt(str.indexOf('x')))
}

{
  val r = Range(1, 10).map(i => BigInt(i))
  r.foreach { rr =>
    println(rr)
    println(sqrtN(rr * rr))
  }
}

{
  val square = new LinearSquare(9)
  Range(0, 9).foreach { i =>
    println("--")
    println(square.xy(i))
    val right = square.right(i)
    val rightXY = right.map(r => square.xy(r))
    println(s"right=$right ($rightXY)")
    val down = square.down(i)
    val downXY = down.map(d => square.xy(d))
    println(s"down=$down ($downXY)")
  }

}


{
  //   00 0 even
  //   01 1  odd
  //   10 2 even
  //   11 3  odd
  //  100 4 even
  //  101 5  odd
  //  110 6 even
  //  111 7  odd
  // 1000 8 even

  Range(0, 9).foreach { i =>
    println(s"i=$i: ${even(i)}")
  }
}

{
  val list = List(
    Vector(2, 1, 1, 0),
    Vector(2, 2, 0, 0)
  )

  list.foreach { l =>
    println(l)
    val f = factoradic(l)
    println(f)
    val r = radix(f, List(factorial(3), factorial(2), factorial(1), factorial(0)))
    println(r)
  }

  Range(0, 16).foreach { n =>
    println("--")
    println(s"n=$n")
    println(radix(n, List(1 << 3, 1 << 2, 1 << 1, 1 << 0)))
    println("0x" + n.toBinaryString)
  }
}


{
  val digits = Range.inclusive('0', '9').map(c => (c - '0').toString)
  println(digits)
}

{
  val list = "aabbbcccc"
  var bag = Bag.of(list.toList)
  println(bag.count('a'))
  bag = bag.add('a', 6)
  println(bag.count('a'))
  bag = bag.del('a', 3)
  println(bag.count('a'))
}

{
  vec(-1, 0, 1).foreach { dy =>
    vec(-1, 0, 1).foreach { dx =>
      if (dy.abs != dx.abs) {
        println(" " + dy + " " + dx)

      }
    }
  }
}


{
  val l0 = List(1, 2, 3)
  val l1 = List(9, 8, 7, 6, 5)
  println(l0.zip(l1))

}