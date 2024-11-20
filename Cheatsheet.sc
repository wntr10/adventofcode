
val sub = List(0, 1, 2)
val list = sub ++ sub ++ sub ++ sub
list.zipWithIndex.foreach {
  case (e, i) =>
    require(e == i % sub.length)
}
