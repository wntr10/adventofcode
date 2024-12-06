
final class Bag[T](val delegate: Map[T, BigInt]) {

  override def toString = delegate.toString()

  def get(element: T): BigInt = {
    delegate.getOrElse(element, BigInt(0))
  }

  def count(element: T): BigInt = get(element)

  def values(): Iterable[BigInt] = {
    delegate.values
  }

  def sumBy(pred: T => Boolean): BigInt = {
    delegate.filter(e => pred(e._1)).values.sum
  }

  def filter(pred: T => Boolean): Bag[T] = {
    new Bag(delegate.filter(e => pred(e._1)))
  }

  def addAll(elements: T*): Bag[T] = {
    var prime = delegate
    elements.foreach { element =>
      val c = prime.getOrElse(element, BigInt(0))
      prime = prime.updated(element, c + 1)
    }
    new Bag[T](prime)
  }

  def add(element: T, times: BigInt = BigInt(1)): Bag[T] = {
    require(times > 0)
    val c = delegate.getOrElse(element, BigInt(0))
    val prime = delegate.updated(element, c + times)
    new Bag[T](prime)
  }

  def delExact(element: T, times: BigInt = BigInt(1)): Bag[T] = {
    val c = delegate.getOrElse(element, BigInt(0))
    if (c > 0) {
      val cPrime = c - times
      val prime = if (cPrime > 0) {
        delegate.updated(element, cPrime)
      } else {
        require(c == 0, c)
        delegate - element
      }
      new Bag[T](prime)
    } else {
      this
    }
  }

  def del(element: T, times: BigInt = BigInt(1)): Bag[T] = {
    val c = delegate.getOrElse(element, BigInt(0))
    if (c > 0) {
      val cPrime = c - times
      val prime = if (cPrime > 0) {
        delegate.updated(element, cPrime)
      } else {
        delegate - element
      }
      new Bag[T](prime)
    } else {
      this
    }
  }


}

object Bag {

  def empty[T]: Bag[T] = {
    new Bag[T](Map.empty[T, BigInt])
  }

  def of[T](list: Seq[T]): Bag[T] = {
    var bag = new Bag[T](Map.empty[T, BigInt])
    list.foreach { e =>
      bag = bag.add(e)
    }
    bag
  }
}
