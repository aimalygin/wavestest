import scala.io.Source

final case class IncorrectStringException(message: String) extends RuntimeException(message)

trait Parser[A] {
  def fromString(string: String): A

  def split(string: String, size: Int): Seq[String] = {
    val fields = string.split("\t")
    if (fields.length != size)
      throw IncorrectStringException(s"Incorrect string: $string")
    fields
  }
}

object ParsableString {

  implicit class StringAs(value: String) {
    def parse[T](implicit parser: Parser[T]): T = parser.fromString(value)
  }
}

class ParsableFile(file: String) {

  import ParsableString._

  def parseAs[T](implicit parser: Parser[T]): Iterator[T] = {
    Source.fromResource(file).getLines.map(_.parse)
  }
}
