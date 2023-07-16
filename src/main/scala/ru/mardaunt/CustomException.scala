package ru.mardaunt

object CustomException {

  case object ClientBadFormatException extends Exception("Плохой формат записи о клиенте")
  case object OrderBadFormatException extends Exception("Плохой формат записи о сделке")

}
