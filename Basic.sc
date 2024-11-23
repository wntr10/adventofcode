import com.google.common.base.{Charsets, Splitter}
import com.google.common.io.Files

import java.io.File
import scala.jdk.CollectionConverters.CollectionHasAsScala

object Input {
  def read(name: String): String = {
    val file = new File(name)
    Files.asCharSource(file, Charsets.UTF_8).read() + "\n"
  }

  def split(c: String, input: String) = {
    Splitter
      .on(c)
      .omitEmptyStrings()
      .trimResults()
      .splitToList(input)
      .asScala
      .toList

  }
}

class Bag[T](delegate: Map[T, BigInt]) {

  override def toString = delegate.toString()

  def sumBy(pred: T => Boolean): BigInt = {
    delegate.filter(e => pred(e._1)).values.sum
  }

  def filter(pred: T => Boolean): Bag[T] = {
    new Bag(delegate.filter(e => pred(e._1)))
  }

  def addAll(elements: T*): Bag[T] = {
    var prime = delegate
    elements.foreach { element =>
      val c = delegate.getOrElse(element, BigInt(0))
      prime = delegate.updated(element, c + 1)
    }
    new Bag[T](prime)
  }

  def add(element: T, times: BigInt = BigInt(1)): Bag[T] = {
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

  def count(element: T): BigInt = {
    delegate.getOrElse(element, BigInt(0))
  }
}

object Bag {

  def empty[T]: Bag[T] = {
    new Bag[T](Map.empty[T, BigInt])
  }

  def of[T](list: List[T]): Bag[T] = {
    var bag = new Bag[T](Map.empty[T, BigInt])
    list.foreach { e =>
      bag = bag.add(e)
    }
    bag
  }
}
