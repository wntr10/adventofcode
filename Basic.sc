import com.google.common.base.Charsets
import com.google.common.io.Files
import java.io.File

object Input {
    def read(name: String): String = {
        val file = new File(name)
        Files.asCharSource(file, Charsets.UTF_8).read()
    }
}
