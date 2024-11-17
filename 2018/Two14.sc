
val reach = "59414" // 2018

var eval = "37"

var a = 0 // current recipe of the first elf
var b = 1

val array = Array.ofDim[Byte](1 << 25) // big enough for my input

array(0) = 3
array(1) = 7

var i = 2

var stop = false

while (!stop) {

  val av = array(a)
  val bv = array(b)

  // create new recipes
  val prime = ("" + av + bv).map(_.toString.toInt).sum.toString

  prime.foreach { nr =>
    array(i) = nr.toString.toByte
    i = i + 1
    eval = eval + nr
    if (eval.endsWith(reach)) {
      println(i - reach.length)
      stop = true
    }
  }
  eval = eval.drop(eval.length - reach.length)

  a = (a + av + 1) % i
  b = (b + bv + 1) % i
}
