import scala.collection.mutable.{Map => MutableMap}

case class Client(name: String, var balance: Int, instrumentsCount: MutableMap[String, Int]) {
  override def toString: String = {
    s"$name\t$balance\t${instrumentsCount.toSeq.sortBy(_._1).map(_._2.toString).mkString("\t")}"
  }
}
