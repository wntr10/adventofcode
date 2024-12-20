
sealed trait Tree[+A] {
  def depth: Int
}

object List {
  case class Leaf[+A](value: A) extends Tree[A] {
    override def depth: Int = 0
  }

  case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A] {
    override def depth: Int = 1 + left.depth.max(right.depth)
  }
}
