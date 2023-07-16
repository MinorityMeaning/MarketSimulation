package ru.mardaunt

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TradingMarketTests extends AnyFunSuite {

  test("Попробуем получить файл с результом для тестовых данных") {

    val resultFilePath =
      new TradingMarket().startSimulation("test_clients.txt", "test_orders.txt", "test_result.txt")

    val resultFile = resultFilePath.toFile

    assert(resultFile.isFile)

    val source = Source.fromFile(resultFile)

    val expectedData = List(List("Михалыч","2600","5","5","5","5"),List("Николай","1600","5","5","5","5"))
    val actualData = source.getLines().map(_.split("\t").toList).toList

    source.close

    assertResult(expectedData)(actualData)
  }

  test("Попрубуем получить результат, если файл с профилями клиента пустой") {

    val resultFilePath =
      new TradingMarket().startSimulation("empty_clients.txt", "test_orders.txt", "test_result.txt")

    val resultFile = resultFilePath.toFile

    assert(resultFile.isFile)

    val source = Source.fromFile(resultFile)

    val actualData = source.getLines().map(_.split("\t").toList).toList

    source.close

    assertResult(Nil)(actualData)
  }

  test("Попрубуем получить результат, если файл со сделками пустой") {

    val resultFilePath =
      new TradingMarket().startSimulation("test_clients.txt", "empty_orders.txt", "test_result.txt")

    val resultFile = resultFilePath.toFile

    assert(resultFile.isFile)

    val source = Source.fromFile(resultFile)

    val expectedData = List(List("Михалыч","2000","10","10","10","10"),List("Николай","1000","10","10","10","10"))
    val actualData = source.getLines().map(_.split("\t").toList).toList

    source.close

    assertResult(expectedData)(actualData)
  }

  test("Попробуем получить ошибку, если получим плохую запись в данных клиентов или сделок") {
    import ru.mardaunt.CustomException.{ClientBadFormatException, OrderBadFormatException}

    assertThrows[ClientBadFormatException.type](
      new TradingMarket().startSimulation("brock_clients.txt", "test_orders.txt", "test_result.txt")
    )
    assertThrows[OrderBadFormatException.type](
      new TradingMarket().startSimulation("test_clients.txt", "brock_orders.txt", "test_result.txt")
    )
  }
}
