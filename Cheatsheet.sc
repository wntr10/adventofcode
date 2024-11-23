import $file.Basic, Basic._

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
  val list = "aabbbcccc"
  var bag = Bag.of(list.toList)
  println(bag.count('a'))
  bag = bag.add('a', 6)
  println(bag.count('a'))
  bag = bag.del('a', 3)
  println(bag.count('a'))
}
