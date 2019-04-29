import ParserImplicits._

object App {
  def main(args: Array[String]): Unit = {
    val orders = new ParsableFile("orders.txt").parseAs[Order]


    val clients = new ParsableFile("clients.txt").parseAs[Client].toList.map(rs => rs.name -> rs).toMap

    val matching = new OrdersMatching(clients)
    orders.foreach(matching.executionOrder)
    Utils.writeFile("results.txt", matching.resultText)
  }
}
