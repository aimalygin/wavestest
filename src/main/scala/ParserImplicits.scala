object ParserImplicits {

  implicit val clientsParser = new Parser[Client] {

    override def fromString(string: String): Client = {
      val fields = split(string, 6)

      val instrumentsCount = fields.drop(2).zipWithIndex.map {
        case (v, 0) => "A" -> v.toInt
        case (v, 1) => "B" -> v.toInt
        case (v, 2) => "C" -> v.toInt
        case (v, 3) => "D" -> v.toInt
        case _ => throw IncorrectStringException(s"Incorrect string: $string")
      }.toMap

      Client(fields.head, fields(1).toInt, collection.mutable.Map(instrumentsCount.toSeq: _*))
    }
  }

  implicit val bidsParser = new Parser[Order] {

    override def fromString(string: String): Order = {
      val fields = split(string, 5)

      Order(fields.head, Action.withName(fields(1)), fields(2), fields(3).toInt, fields(4).toInt)
    }
  }
}
