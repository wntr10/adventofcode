
sealed trait List[+A] {
  def size: Int
}

object List {
  case class Cons[+A](head: A, tail: List[A]) extends List[A] {

    override def size: Int = 1 + tail.size
  }
  case object Nil extends List[Nothing] {

    override def size: Int = 0
  }
}
