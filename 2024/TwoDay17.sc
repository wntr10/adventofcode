// Specific for the following input
// Register A: 64012472
// Register B: 0
// Register C: 0
//
// Program: 2,4,1,7,7,5,0,3,1,7,4,1,5,5,3,0

def func(a: BigInt): Int = {
  var B = (a & 7) ^ 7
  val C = a / (BigInt(1) << B.toInt)
  B = (B ^ 7) ^ C
  val o = B & 7
  o.toInt
}

def solveRec(init: BigInt, oracle: List[Int], todo: List[Int], solution: List[(BigInt, Int)] = List.empty): Unit = {
  if (todo.isEmpty) {
    println(solution)
    return
  }

  val op = todo.head :: oracle
  val solutions = solve(init, op)
  solutions.foreach { s =>
    solveRec((init + s) << 3, op, todo.drop(1), (init + s, s) :: solution)
  }
}

def solve(init: BigInt, oracle: List[Int]): List[Int] = {
  var c = 0
  var r = List.empty[Int]
  while (true) {
    val cl = check(init + c, oracle)
    if (cl == oracle) {
      r = c :: r
      return r.reverse
    }
    c = c + 1
  }
  r
}

def check(init: BigInt, l: List[Int]): List[Int] = {
  var cl = l
  var ca = init
  var result = List.empty[Int]
  while (true) {
    val o = func(ca)
    result = o :: result

    if (cl.nonEmpty) {
      if (o != cl.head) {
        return result.reverse
      }
      cl = cl.drop(1)
    }
    ca = ca >> 3
    if (ca == 0) {
      return result.reverse
    } else if (cl.isEmpty && ca != 0) {
      return result.reverse
    }
  }
  result
}

solveRec(7, List.empty, List(2, 4, 1, 7, 7, 5, 0, 3, 1, 7, 4, 1, 5, 5, 3, 0).reverse)
