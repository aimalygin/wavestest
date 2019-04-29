import scala.collection.mutable.{ListBuffer, Map => MutableMap}

final case class CompleteOrderException(message: String) extends RuntimeException(message)

class OrdersMatching(clients: Map[String, Client]) {

  private val clientLogic: ClientBalances = new ClientBalances(clients)

  val instrumentsToOrdersList: MutableMap[String, ListBuffer[Order]] = MutableMap.empty

  def results: Map[String, Client] = clientLogic.clients

  def resultText: String = clientLogic.clients.toSeq.sortBy(_._1).map(_._2.toString).mkString("\n")

  def executionOrder(order: Order): Unit = {
      matchingOrder(order)
  }

  private def canCompleteOrder(order: Order): Boolean = {
    clients.get(order.clientName) match {
      case Some(client) if order.action == Action.buy =>
        client.balance >= (order.price * order.amount)
      case Some(client) if order.action == Action.sale =>
        client.instrumentsCount.get(order.instrument) match {
          case Some(instrumentCount) => instrumentCount >= order.amount
          case None => throw CompleteOrderException(s"Missing trading instrument: ${order.instrument}")
        }
      case None => throw CompleteOrderException(s"Missing client: ${order.clientName}")
    }
  }

  private def matchingOrder(order: Order): Unit = {
    instrumentsToOrdersList.get(order.instrument) match {
      case Some(ordersList) =>
        recursionMatchAndUpdate(ordersList, order)
      case None =>
        instrumentsToOrdersList += order.instrument -> ListBuffer(order)
    }
  }

  private def recursionMatchAndUpdate(ordersList: ListBuffer[Order], order: Order): Option[Order] = {
    val firstFindOrder = ordersList.find(rs => rs.action != order.action && priceCondition(rs, order))
    if (firstFindOrder.isDefined && canCompleteOrder(firstFindOrder.get) && canCompleteOrder(order)) {
      clientLogic.update(firstFindOrder.get, order)

      val newOrder = updateCompletedOrdersList(ordersList, firstFindOrder.get, order)
      if (newOrder.isDefined)
        recursionMatchAndUpdate(ordersList, newOrder.get)
      else None
    } else {
      ordersList += order
      Some(order)
    }
  }

  private def priceCondition(left: Order, right: Order): Boolean = {
    if (left.action == Action.buy)
      left.price >= right.price
    else
      left.price <= right.price
  }

  private def updateCompletedOrdersList(ordersList: ListBuffer[Order], order1: Order, order2: Order): Option[Order] = {
    val diff = order1.amount - order2.amount
    diff match {
      case 0 =>
        ordersList -= order1
        ordersList -= order2
        None
      case x if x > 0 =>
        order1.amount = diff
        None
      case x if x < 0 =>
        ordersList -= order1
        val newOrder = order2.copy(amount = -diff)
        Some(newOrder)
    }
  }

}
