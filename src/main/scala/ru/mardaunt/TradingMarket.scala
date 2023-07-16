package ru.mardaunt

import ru.mardaunt.CustomException.{ClientBadFormatException, OrderBadFormatException}

import java.nio.file.{Files, Path, Paths}
import scala.annotation.tailrec
import scala.io.Source

class TradingMarket {

  /** Типы для ценных бумаг. */
  sealed trait SEC
    case object A extends SEC
    case object B extends SEC
    case object C extends SEC
    case object D extends SEC


  /** Типы для операций покупки/продажи. */
  sealed trait Operation
    case class Buy (sec: SEC, price: Int, count: Int) extends Operation
    case class Sale(sec: SEC, price: Int, count: Int) extends Operation

  /** Структура сделки. */
  case class Order(clientName: String, operation: Operation)

  /** Структура профиля клиента. */
  case class Profile(clientName: String, dollars: Int, aCount: Int, bCount: Int, cCount: Int, dCount: Int){
    override def toString = s"$clientName\t$dollars\t$aCount\t$bCount\t$cCount\t$dCount"
  }


  /** Запустить симуляцию торгов для клиентов
   * @param clientSourceFile Имя файла с данными баланса клиентов.
   * @param orderSourceFile Имя файла со сделками клиентов.
   */
  def startSimulation(clientSourceFile: String = "clients.txt",
                      orderSourceFile: String  = "orders.txt",
                      resultFile: String       = "result.txt"): Path = {
    val clientOrders: Map[String, List[Order]] = readOrders(orderSourceFile).groupBy(_.clientName)

    val resultProfileList = readTXT(clientSourceFile) map {
      case List(clientName, dollars, aCount, bCount, cCount, dCount) =>
        getResultClientProfile(
          clientOrders.getOrElse(clientName, Nil),
          clientName,
          dollars.toInt,
          aCount.toInt,
          bCount.toInt,
          cCount.toInt,
          dCount.toInt
        )
      case badRecord => println(s"Плохая запись сделки: $badRecord")
                        throw ClientBadFormatException
    }

    Files.write(Paths.get(System.getProperty("user.dir"), resultFile), resultProfileList.mkString("\n").getBytes)
  }

  /** Получить итоговый профиль клиента после всех сделок.
   * @param orderList Список всех сделок клиента.
   * @param clientName Имя клиента.
   * @param dollars Баланс долларов на момент начала торгов.
   * @param aCount Баланс A на момент начала торгов.
   * @param bCount Баланс B на момент начала торгов.
   * @param cCount Баланс C на момент начала торгов.
   * @param dCount Бананс D на момент начала торгов.
   * @return Экземпляр Profile.
   */
  private def getResultClientProfile(orderList: List[Order],
                                     clientName: String,
                                     dollars: Int,
                                     aCount: Int,
                                     bCount: Int,
                                     cCount: Int,
                                     dCount: Int): Profile = {
    @tailrec
    def loop(orderList: List[Order], dollars: Int, aCount: Int, bCount: Int, cCount: Int, dCount: Int): Profile =
      orderList match {
        case Nil => Profile(clientName, dollars, aCount, bCount, cCount, dCount)

        case List(Order(_, Buy (A, price, count)), _*) => loop(orderList.tail, dollars - (price * count), aCount + count, bCount, cCount, dCount)
        case List(Order(_, Sale(A, price, count)), _*) => loop(orderList.tail, dollars + (price * count), aCount - count, bCount, cCount, dCount)

        case List(Order(_, Buy (B, price, count)), _*) => loop(orderList.tail, dollars - (price * count), aCount, bCount + count, cCount, dCount)
        case List(Order(_, Sale(B, price, count)), _*) => loop(orderList.tail, dollars + (price * count), aCount, bCount - count, cCount, dCount)

        case List(Order(_, Buy (C, price, count)), _*) => loop(orderList.tail, dollars - (price * count), aCount, bCount, cCount + count, dCount)
        case List(Order(_, Sale(C, price, count)), _*) => loop(orderList.tail, dollars + (price * count), aCount, bCount, cCount - count, dCount)

        case List(Order(_, Buy (D, price, count)), _*) => loop(orderList.tail, dollars - (price * count), aCount, bCount, cCount, dCount + count)
        case List(Order(_, Sale(D, price, count)), _*) => loop(orderList.tail, dollars + (price * count), aCount, bCount, cCount, dCount - count)
      }

    loop(orderList, dollars, aCount, bCount, cCount, dCount)
  }

  /** Прочитать сделки из файла.
   * @param txtSource Название файла.
   * @return Список всех сделок.
   */
  private def readOrders(txtSource: String): List[Order] =
    readTXT(txtSource).map(createOrder).toList

  /** Создать экземпляр Order.
   * @param record Запись одной сделки.
   * @return Экземпляр Order.
   */
  private def createOrder(record: List[String]): Order = record match {
      case List(clientName, _, _, _, _) => Order(clientName, createOperation(record))
      case _ => println(s"Плохая запись сделки: $record")
                throw OrderBadFormatException
    }

  /** Создать экземпляр операции покупки/продажи Operation.
   * @param record Запись одной сделки.
   * @return Экземпляр Operation.
   */
  private def createOperation(record: List[String]): Operation = record match {
    case List(_, "b", sec, price, count) => Buy (createSEC(sec), price.toInt, count.toInt)
    case List(_, "s", sec, price, count) => Sale(createSEC(sec), price.toInt, count.toInt)
  }

  /** Вернуть экземпляр типа ценной бумаги.
   * @param sec Название ценной бумаги в формате String.
   * @return Экземпляр ценной бумаги.
   */
  private def createSEC(sec: String): SEC = sec match {
    case "A" => A
    case "B" => B
    case "C" => C
    case "D" => D
  }

  /** Прочитать TXT файл.
   * @param txtSource Название TXT файла.
   * @return Список записей из файла.
   */
  private def readTXT(txtSource: String): Iterator[List[String]] = {
    val source = Source.fromInputStream(getClass.getResourceAsStream(txtSource))
    val recordList = source.getLines().map(_.split("\t").toList)
    recordList
  }

}
