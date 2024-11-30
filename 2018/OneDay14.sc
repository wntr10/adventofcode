
var board = "37"

var a = 0 // current recipe of the first elf
var b = 1

val reach = 2018 // 5941429882
var todo = ""
var after = false

while (todo.length < 10) {

  val av = board.charAt(a).toString.toInt
  val bv = board.charAt(b).toString.toInt

  // create new recipes
  val prime = ("" + av + bv).map(_.toString.toInt).sum.toString

  prime.foreach { nr =>
    board = board + nr
    if (after) {
      todo = todo + nr
    }
    if (board.length == reach) {
      after = true
    }
  }

  a = (a + av + 1) % board.length
  b = (b + bv + 1) % board.length
}

println(todo)
