package csdbase.descriptor.parameter

import org.scalatest.FlatSpec
import io.circe._, io.circe.parser.parse

class ParameterDecodingSpec extends FlatSpec {

  "Parameter decoder" should "decode boolean parameters" in {

    val json =
      """
        |{
        | "name": "foo",
        | "label": "Foo",
        | "description": "Foo Parameter",
        | "required": false,
        | "default": false,
        | "type": "boolean"
        |}
      """.stripMargin

    val result = parse(json) flatMap {
      parsed => parsed.as[Parameter[_]]
    }

    assert(result.isRight)
    assert(result.exists(_.isInstanceOf[BooleanParameter]))

  }

}
