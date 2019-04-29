import org.scalatest.{FlatSpec, Matchers}
import ParserImplicits._
import scala.collection.mutable.{Map => MutableMap}

class AppSpec extends FlatSpec with Matchers {

  import ParsableString._

  "Parse Client" should "success" in {

    "C1\t1000\t130\t240\t760\t320".parse[Client] shouldBe Client("C1", 1000, MutableMap("A" -> 130, "B" -> 240, "C" -> 760, "D" -> 320))

  }

  "Parse Client" should "fail" in {

    intercept[IncorrectStringException] {
      "1000\t130\t240\t760\t320".parse[Client]
    }
  }

  "Parse clients list" should "non empty" in {
    val file = new ParsableFile("clients.txt")
    val clients = file.parseAs[Client].toList
    clients should not be empty
  }

  "Parse Order" should "success" in {

    "C8\tb\tC\t15\t4".parse[Order] shouldBe Order("C8", Action.buy, "C", 15, 4)

  }

  "Parse Order" should "fail" in {

    intercept[IncorrectStringException] {
      "\tC\t15\t4".parse[Order]
    }

  }

  "Parse orders list" should "non empty" in {
    val file = new ParsableFile("orders.txt")
    val orders = file.parseAs[Order].toList
    orders should not be empty
  }

  "Matching orders" should "success if C1 buy C2 sale and partial matching orders" in {
    val orders = List(
      Order("C1", Action.buy, "A", 10, 3),
      Order("C2", Action.sale, "A", 8, 2)
    )


    val clients = Map(
      "C1" -> Client("C1", 100, MutableMap("A" -> 20)),
      "C2" -> Client("C2", 50, MutableMap("A" -> 20))
    )

    val matching = new OrdersMatching(clients)
    orders.foreach(matching.executionOrder)
    val result = matching.results.map {case (_, v) => v}
    result.head shouldBe Client("C1", 84, MutableMap("A" -> 22))
    result.last shouldBe Client("C2", 66, MutableMap("A" -> 18))
  }

  "Matching orders" should "success if C1 sale C2 buy and partial matching orders" in {
    val orders = List(
      Order("C1", Action.sale, "A", 9, 3),
      Order("C3", Action.sale, "A", 10, 1),
      Order("C2", Action.buy, "A", 10, 5),
      Order("C4", Action.sale, "A", 7, 1),
      Order("C2", Action.sale, "A", 15, 5),
      Order("C4", Action.buy, "A", 15, 2)
    )


    val clients = Map(
      "C1" -> Client("C1", 100, MutableMap("A" -> 20)),
      "C2" -> Client("C2", 50, MutableMap("A" -> 20)),
      "C3" -> Client("C3", 50, MutableMap("A" -> 20)),
      "C4" -> Client("C4", 100, MutableMap("A" -> 20))
    )

    val matching = new OrdersMatching(clients)
    orders.foreach(matching.executionOrder)
    val result = matching.results.map {case (_, v) => v}.toList
    result.head shouldBe Client("C1", 127, MutableMap("A" -> 17))
    result(1) shouldBe Client("C2", 36, MutableMap("A" -> 23))
    result(2) shouldBe Client("C3", 60, MutableMap("A" -> 19))
    result(3) shouldBe Client("C4", 77, MutableMap("A" -> 21))
    matching.instrumentsToOrdersList("A").head shouldBe Order("C2", Action.sale, "A", 15, 3)
  }

  "Matching orders" should "client have positive balance" in {
    val orders = new ParsableFile("orders.txt").parseAs[Order].drop(2000)


    val clients = new ParsableFile("clients.txt").parseAs[Client].toList.map(rs => rs.name -> rs).toMap

    val matching = new OrdersMatching(clients)
    orders.foreach(matching.executionOrder)
    matching.results.map {
      case (_, client) => client.balance should be >= 0
    }
  }

  "Matching orders" should "save results.txt" in {
    val orders = new ParsableFile("orders.txt").parseAs[Order]


    val clients = new ParsableFile("clients.txt").parseAs[Client].toList.map(rs => rs.name -> rs).toMap

    val matching = new OrdersMatching(clients)
    orders.foreach(matching.executionOrder)
    Utils.writeFile("results.txt", matching.resultText)
    succeed
  }

}
