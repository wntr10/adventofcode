package wntr10.adventofcode

import com.google.common.io.CharStreams

import java.io.{BufferedReader, InputStream, InputStreamReader, Reader}

final class Data(name: String) {

  def read: String = {
    println(s"name=<$name>")
    val action = new AutoAction[InputStream, String](() => getClass.getClassLoader.getResourceAsStream(name))
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
