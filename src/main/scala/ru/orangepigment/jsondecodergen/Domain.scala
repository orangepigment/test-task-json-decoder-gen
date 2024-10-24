package ru.orangepigment.jsondecodergen

/**
 * Исходные данные:
 * Упрощенная модель для представления JSON объекта.
 */
object Domain {
  sealed trait JVal

  sealed trait JType

  case class JStr(str: String) extends JVal

  object JStr extends JType

  case class JBool(bool: Boolean) extends JVal

  object JBool extends JType

  case class JNum(long: Long) extends JVal

  object JNum extends JType

  type JsonObj = Map[String, JVal]

  /**
   * Необходимо написать функцию генерации JSON декодера, принимающую на вход описание в формате:
   * ("name" -> JStr) :: ("active" -> JBool) :: ("age" -> JNum) :: Nil : List[(String, JType)]
   * и возвращающую декодер типа Decoder.
   */
  case class Error(cause: String)

  type DecoderResult = Either[Error, Map[String, JVal]]

  type Decoder = Map[String, String] => DecoderResult
}