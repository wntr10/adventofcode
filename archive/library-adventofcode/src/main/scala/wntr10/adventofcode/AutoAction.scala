package wntr10.adventofcode

import java.io.Closeable
import scala.util.{Failure, Success, Try}

final class AutoAction[A <: Closeable, B](factory: () => A) {
  def exec(f: A => B): B = {
    val resource = factory()
    Try(f(resource)) match {
      case Success(result) =>
        resource.close()
        result
      case Failure(e) =>
        resource.close()
        throw e
    }
  }
}
