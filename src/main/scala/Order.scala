

case class Order(clientName: String, action: Action.Value, instrument: String, price: Int, var amount: Int)
