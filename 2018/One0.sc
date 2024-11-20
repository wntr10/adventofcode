import $ivy.`org.jgrapht:jgrapht-core:1.5.2`
import $ivy.`com.google.guava:guava:33.3.1-jre`
import $file.^.Basic
import Basic._
import com.google.common.base.Splitter
import scala.jdk.CollectionConverters._

val ex = ""

val input = Input.read(s"day0$ex")

val lines = Splitter.on("\n").omitEmptyStrings().trimResults().splitToList(input).asScala

lines.foreach { line =>
  println(line)
}
