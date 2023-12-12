package wntr10.adventofcode

import java.io.{BufferedReader, InputStream, InputStreamReader, Reader}

import com.google.common.io.CharStreams

final class Input(name: String, suffix: String = "") {
  require(name.nonEmpty && (name.endsWith("PartOne$") || name.endsWith("PartTwo$") || name.endsWith("PartAlt$")))

  def read: String = {
    val namePrime = name.substring(0, name.length - ".PartOne$".length) + suffix
    println(s"name=<$name> namePrime=<$namePrime>")
    val action = new AutoAction[InputStream, String](() => getClass.getClassLoader.getResourceAsStream(namePrime))
    val content = action exec { in =>
      val sub = new AutoAction[Reader, String](() => new BufferedReader(new InputStreamReader(in)))
      sub exec { reader =>
        CharStreams.toString(reader)
      }
    }
    require(content.endsWith("\n"))
    content
  }
}
