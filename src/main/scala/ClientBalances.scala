import scala.math.min

class ClientBalances(val clients: Map[String, Client]) {

  def update(order1: Order, order2: Order) = {
    val (updateOrder1, updateOrder2) = ordersForUpdateClient(order1, order2)
    updateClientBalance(updateOrder1)
    updateClientBalance(updateOrder2)
  }

  private def ordersForUpdateClient(order1: Order, order2: Order): (Order, Order) = {
    val price = min(order1.price, order2.price)
    val amount = min(order1.amount, order2.amount)
    order1.copy(price = price, amount = amount) -> order2.copy(price = price, amount = amount)
  }

  private def updateClientBalance(order: Order) = {
    clients.get(order.clientName).foreach { client =>
      order.action match {
        case Action.buy =>
          val currentAmount = client.instrumentsCount
            .getOrElse(order.instrument, throw CompleteOrderException(s"Missing trading instrument at the client ${client.name}"))

          client.instrumentsCount.update(order.instrument, currentAmount + order.amount)
          client.balance -= (order.price * order.amount)
        case Action.sale =>
          val currentAmount = client.instrumentsCount
            .getOrElse(order.instrument, throw CompleteOrderException(s"Missing trading instrument at the client ${client.name}"))

          client.instrumentsCount.update(order.instrument, currentAmount - order.amount)
          client.balance += (order.price * order.amount)
      }
    }
  }
}
