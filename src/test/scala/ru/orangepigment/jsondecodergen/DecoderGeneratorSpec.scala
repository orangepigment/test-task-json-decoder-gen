package ru.orangepigment.jsondecodergen

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import ru.orangepigment.jsondecodergen.Domain._

class DecoderGeneratorSpec extends AnyFlatSpec with should.Matchers {

  private val commonScheme: List[(String, JType)] =
    ("name" -> JStr) :: ("active" -> JBool) :: ("age" -> JNum) :: Nil
  private val commonJson =
    Map("name" -> "Ivan", "active" -> "true", "age" -> "27")

  "DecoderGenerator.generateDecoder" should "generate empty decoder" in {
    val decoder = DecoderGenerator.generateDecoder(List.empty)
    val expected = Right[Error, Map[String, JVal]](Map.empty[String, JVal])
    decoder(commonJson) should be(expected)
  }

  it should "generate working decoder" in {
    val decoder = DecoderGenerator.generateDecoder(commonScheme)
    val expected = Right[Error, Map[String, JVal]](
      Map(
        "name" -> JStr("Ivan"),
        "active" -> JBool(true),
        "age" -> JNum(27)
      )
    )
    decoder(commonJson) should be(expected)
  }

  it should "generate decoder which produces errors if a json with missing fields is passed to it" in {
    val decoder = DecoderGenerator.generateDecoder(commonScheme)
    val expected = Left[Error, Map[String, JVal]](
      Error(
        "Key \"active\" not found"
      )
    )
    val invalidJson = Map("name" -> "Ivan", "age" -> "27")
    decoder(invalidJson) should be(expected)
  }

  it should "generate decoder which produces errors if a json with invalid value type  is passed to it" in {
    val decoder = DecoderGenerator.generateDecoder(commonScheme)
    val expected = Left[Error, Map[String, JVal]](
      Error(
        "Failed to parse value \"twenty seven\" as long at key \"age\""
      )
    )
    val invalidJson = Map(
      "name" -> "Ivan",
      "active" -> "true",
      "age" -> "twenty seven"
    )
    decoder(invalidJson) should be(expected)
  }

  it should "generate working decoder which does not produce errors if a json contains unexpected keys" in {
    val decoder = DecoderGenerator.generateDecoder(commonScheme)
    val expected = Right[Error, Map[String, JVal]](
      Map(
        "name" -> JStr("Ivan"),
        "active" -> JBool(true),
        "age" -> JNum(27)
      )
    )
    val extendedJson = Map(
      "name" -> "Ivan",
      "active" -> "true",
      "age" -> "27",
      "city" -> "Moscow",
      "department" -> "IT"
    )
    decoder(extendedJson) should be(expected)
  }

  "DecoderGenerator.generateStrictDecoder" should "generate decoder which produces errors if a json contains unexpected keys" in {
    val decoder = DecoderGenerator.generateStrictDecoder(commonScheme)
    val expected = Left[Error, Map[String, JVal]](
      Error(
        "JSON contains unexpected keys: city, department"
      )
    )
    val invalidJson = Map(
      "name" -> "Ivan",
      "active" -> "true",
      "age" -> "27",
      "city" -> "Moscow",
      "department" -> "IT"
    )
    decoder(invalidJson) should be(expected)
  }

  it should "generate working decoder" in {
    val decoder = DecoderGenerator.generateStrictDecoder(commonScheme)
    val expected = Right[Error, Map[String, JVal]](
      Map(
        "name" -> JStr("Ivan"),
        "active" -> JBool(true),
        "age" -> JNum(27)
      )
    )
    decoder(commonJson) should be(expected)
  }

}
