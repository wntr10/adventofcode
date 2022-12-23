package wntr10.adventofcode

import com.google.common.io.CharStreams

import java.io.{BufferedReader, InputStream, InputStreamReader, Reader}

class Input(name: String) {
  def rows: String = {
    println("name " + name)
    val action = new AutoAction[InputStream, String](() => getClass.getClassLoader.getResourceAsStream(name))
    action exec { in =>
      val sub = new AutoAction[Reader, String](() => new BufferedReader(new InputStreamReader(in)))
      sub exec { reader =>
        CharStreams.toString(reader)
      }
    }
  }
}
