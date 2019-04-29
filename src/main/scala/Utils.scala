import java.io._

object Utils {
  def writeFile(name: String, content: String): Unit = {
    val currentDirectory = new java.io.File(".").getCanonicalPath
    val file = new File(s"$currentDirectory/$name")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(content)
    bw.close()
  }
}
