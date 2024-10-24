package ru.orangepigment.jsondecodergen

import ru.orangepigment.jsondecodergen.Domain._

import scala.util.Try

object DecoderGenerator {

  /**
   * Генерирурует декодер для парсинга JSON по заданной схеме.
   *
   * @param description схема JSON для генерации декдоера
   * @return декодер
   */
  def generateDecoder(description: List[(String, JType)]): Decoder =
    description.foldLeft(emptyDecoder) { (decoder, elem) =>
      val (key, valueType) = elem

      { rawJson: Map[String, String] =>
        rawJson.get(key) match {
          case Some(value) =>
            valueType match {
              case JStr =>
                decoder(rawJson).map(
                  decodedJson => decodedJson + (key -> JStr(value))
                )
              case JBool =>
                withUnsafeParse(decoder, rawJson, key, "boolean", value) {
                  JBool(value.toBoolean)
                }
              case JNum =>
                withUnsafeParse(decoder, rawJson, key, "long", value) {
                  JNum(value.toLong)
                }
            }
          case None => Left(Error(s"Key \"$key\" not found"))
        }
      }
    }

  private def withUnsafeParse(
                               decoder: Decoder,
                               rawJson: Map[String, String],
                               key: String,
                               valueType: String,
                               value: String
                             )(parse: => JVal): DecoderResult = {
    Try(parse).fold(
      _ => Left(Error(s"Failed to parse value \"$value\" as $valueType at key \"$key\"")),
      jVal =>
        decoder(rawJson).map(
          decodedJson => decodedJson + (key -> jVal)
        )
    )
  }


  /**
   * Генерирурует декодер для парсинга JSON по заданной схеме.
   * В случае наличия в JSON полей помимо описанных в схеме, декдоер вернет ошибку.
   *
   * @param description схема JSON для генерации декдоера
   * @return декодер
   */
  def generateStrictDecoder(description: List[(String, JType)]): Decoder = {
    val baseDecoder = generateDecoder(description)

    { rawJson =>
      val expectedKeys = description.map(_._1).toSet
      val unexpectedKeys = rawJson.keySet.diff(expectedKeys)
      if (unexpectedKeys.isEmpty) {
        baseDecoder(rawJson)
      } else {
        Left(Error(s"JSON contains unexpected keys: ${unexpectedKeys.mkString(", ")}"))
      }
    }
  }

  private def emptyDecoder: Decoder = _ => Right(Map.empty[String, JVal])

}
