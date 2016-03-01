package csdbase.descriptor.parameter

import java.net.{URISyntaxException, URI}

import io.circe._, io.circe.generic.auto._
import cats.data.Xor, Xor._
import shapeless._

import EncoderInstances._
import DecoderInstances._

sealed abstract class Parameter[T : Encoder : Decoder](
  name: String,
  label: String,
  description: String,
  configName: Option[String],
  required: Option[Boolean],
  default: Option[T],
  configurableInWizard: Option[Boolean],
  sensitive: Option[Boolean],
  `type`: Parameter.ParameterCoproduct)

object Parameter {

  type ParameterCoproduct =
    BooleanParameter :+:
      DoubleParameter :+:
      LongParameter :+:
      MemoryParameter :+:
      PasswordParameter :+:
      PathArrayParameter :+:
      PathParameter :+:
      PortParameter :+:
      StringArrayParameter :+:
      StringEnumParameter :+:
      UriParameter :+:
      UriArrayParameter :+: CNil

  object ParameterCoproduct {
    implicit val AnyParameterDecoder: Decoder[ParameterCoproduct] = Decoder.instance {
      cursor => cursor.downField("type").as[String] flatMap {
        case "boolean" => cursor.as[BooleanParameter] map (Coproduct[ParameterCoproduct](_))
        case "double" => cursor.as[DoubleParameter] map (Coproduct[ParameterCoproduct](_))
        case "long" => cursor.as[LongParameter] map (Coproduct[ParameterCoproduct](_))
        case "memory" => cursor.as[MemoryParameter] map (Coproduct[ParameterCoproduct](_))
        case "port" => cursor.as[PortParameter] map (Coproduct[ParameterCoproduct](_))
        case "string_enum" => cursor.as[StringEnumParameter] map (Coproduct[ParameterCoproduct](_))
        case "string" => cursor.as[StringParameter] map (Coproduct[ParameterCoproduct](_))
        case "password" => cursor.as[PasswordParameter] map (Coproduct[ParameterCoproduct](_))
        case "string_array" => cursor.as[StringArrayParameter] map (Coproduct[ParameterCoproduct](_))
        case "path" => cursor.as[PathParameter] map (Coproduct[ParameterCoproduct](_))
        case "path_array" => cursor.as[PathArrayParameter] map (Coproduct[ParameterCoproduct](_))
        case "uri" => cursor.as[UriParameter] map (Coproduct[ParameterCoproduct](_))
        case "uri_array" => cursor.as[UriArrayParameter] map (Coproduct[ParameterCoproduct](_))
        case other => Left(DecodingFailure(s"Invalid parameter type: $other", cursor.history))
      }
    }
  }

}

case class BooleanParameter(name: String,
  label: String,
  description: String,
  configName: Option[String],
  required: Option[Boolean],
  default: Option[Boolean],
  configurableInWizard: Option[Boolean],
  sensitive: Option[Boolean]) extends Parameter[Boolean](name, label, description, configName, required, default, configurableInWizard, sensitive, types.boolean)

case class DoubleParameter(name: String,
  label: String,
  description: String,
  configName: Option[String],
  required: Option[Boolean],
  default: Option[Double],
  configurableInWizard: Option[Boolean],
  sensitive: Option[Boolean],
  softMin: Option[Double],
  softMax: Option[Double],
  min: Option[Double],
  max: Option[Double],
  unit: Option[ParameterUnit]) extends Parameter[Double](name, label, description, configName, required, default, configurableInWizard, sensitive, types.double)

case class LongParameter(name: String,
  label: String,
  description: String,
  configName: Option[String],
  required: Option[Boolean],
  default: Option[Long],
  configurableInWizard: Option[Boolean],
  sensitive: Option[Boolean],
  softMin: Option[Long],
  softMax: Option[Long],
  min: Option[Long],
  max: Option[Long],
  unit: Option[ParameterUnit]) extends Parameter[Long](name, label, description, configName, required, default, configurableInWizard, sensitive, types.long)

case class MemoryParameter(name: String,
  label: String,
  description: String,
  configName: Option[String],
  required: Option[Boolean],
  default: Option[Long],
  configurableInWizard: Option[Boolean],
  sensitive: Option[Boolean],
  softMin: Option[Long],
  softMax: Option[Long],
  min: Option[Long],
  max: Option[Long],
  unit: Option[ParameterUnit],
  scaleFactor: Option[Double],
  autoConfigShare: Option[Double]) extends Parameter[Long](name, label, description, configName, required, default, configurableInWizard, sensitive, types.memory)

case class PortParameter(name: String,
  label: String,
  description: String,
  configName: Option[String],
  required: Option[Boolean],
  default: Option[Int],
  configurableInWizard: Option[Boolean],
  sensitive: Option[Boolean],
  softMin: Option[Int],
  softMax: Option[Int],
  min: Option[Int],
  max: Option[Int],
  unit: Option[ParameterUnit],
  scaleFactor: Option[Double],
  autoConfigShare: Option[Double]) extends Parameter[Int](name, label, description, configName, required, default, configurableInWizard, sensitive, types.port)

case class StringEnumParameter(name: String,
  label: String,
  description: String,
  configName: Option[String],
  required: Option[Boolean],
  default: Option[String],
  configurableInWizard: Option[Boolean],
  sensitive: Option[Boolean],
  validValues: Seq[String]) extends Parameter[String](name, label, description, configName, required, default, configurableInWizard, sensitive, types.string_enum)

case class StringParameter(name: String,
  label: String,
  description: String,
  configName: Option[String],
  required: Option[Boolean],
  default: Option[String],
  configurableInWizard: Option[Boolean],
  sensitive: Option[Boolean],
  conformRegex: Option[String],
  initType: Option[String]) extends Parameter[String](name, label, description, configName, required, default, configurableInWizard, sensitive, types.string)

case class PasswordParameter(name: String,
  label: String,
  description: String,
  configName: Option[String],
  required: Option[Boolean],
  default: Option[String],
  configurableInWizard: Option[Boolean],
  sensitive: Option[Boolean],
  conformRegex: Option[String],
  credentialProviderCompatible: Option[Boolean],
  alternateScriptParameterName: Option[String]) extends Parameter[String](name, label, description, configName, required, default, configurableInWizard, sensitive, types.password)

case class StringArrayParameter(name: String,
  label: String,
  description: String,
  configName: Option[String],
  required: Option[Boolean],
  default: Option[Seq[String]],
  configurableInWizard: Option[Boolean],
  sensitive: Option[Boolean],
  separator: Option[String],
  minLength: Option[Int],
  maxLength: Option[Int]) extends Parameter[Seq[String]](name, label, description, configName, required, default, configurableInWizard, sensitive, types.string_array)

sealed trait PathType {
  val TypeString = this.toString
}

object PathType {

  case object localDataDir extends PathType
  case object localDataFile extends PathType
  case object serviceSpecific extends PathType

  implicit val encoder : Encoder[PathType] = new Encoder[PathType] {
    def apply(a: PathType) = Json.string(a.toString)
  }

  implicit val decoder : Decoder[PathType] = new Decoder[PathType] {
    def apply(c: HCursor) = c.as[String] flatMap {
      case localDataDir.TypeString => Right(localDataDir)
      case localDataFile.TypeString => Right(localDataFile)
      case serviceSpecific.TypeString => Right(serviceSpecific)
      case other => Left(DecodingFailure(s"Invalid pathType $other", c.history))
    }
  }
}

case class PathParameter(name: String,
  label: String,
  description: String,
  configName: Option[String],
  required: Option[Boolean],
  default: Option[String],
  configurableInWizard: Option[Boolean],
  sensitive: Option[Boolean],
  conformRegex: Option[String],
  pathType: PathType,
  mode: String) extends Parameter[String](name, label, description, configName, required, default, configurableInWizard, sensitive, types.path)

case class PathArrayParameter(name: String,
  label: String,
  description: String,
  configName: Option[String],
  required: Option[Boolean],
  default: Option[Seq[String]],
  configurableInWizard: Option[Boolean],
  sensitive: Option[Boolean],
  separator: Option[String],
  minLength: Option[Int],
  maxLength: Option[Int],
  conformRegex: Option[String],
  pathType: PathType,
  mode: String) extends Parameter[Seq[String]](name, label, description, configName, required, default, configurableInWizard, sensitive, types.path_array)

case class UriParameter(name: String,
  label: String,
  description: String,
  configName: Option[String],
  required: Option[Boolean],
  default: Option[URI],
  configurableInWizard: Option[Boolean],
  sensitive: Option[Boolean],
  conformRegex: Option[String],
  opque: Option[Boolean],
  allowedSchemas: Option[Seq[String]]) extends Parameter[URI](name, label, description, configName, required, default, configurableInWizard, sensitive, types.uri)

case class UriArrayParameter(name: String,
  label: String,
  description: String,
  configName: Option[String],
  required: Option[Boolean],
  default: Option[Seq[URI]],
  configurableInWizard: Option[Boolean],
  sensitive: Option[Boolean],
  separator: Option[String],
  minLength: Option[Int],
  maxLength: Option[Int],
  conformRegex: Option[String],
  opque: Option[Boolean],
  allowedSchemas: Option[Seq[String]]) extends Parameter[Seq[URI]](name, label, description, configName, required, default, configurableInWizard, sensitive, types.uri_array)





