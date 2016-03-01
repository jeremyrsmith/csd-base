package csdbase.descriptor.parameter

import cats.data.Xor.{Left, Right}
import io.circe._

sealed trait ParameterUnit
object ParameterUnit {
  case object milliseconds extends ParameterUnit
  case object seconds extends ParameterUnit
  case object minutes extends ParameterUnit
  case object hours extends ParameterUnit
  case object bytes extends ParameterUnit
  case object kilobytes extends ParameterUnit
  case object megabytes extends ParameterUnit
  case object gigabytes extends ParameterUnit
  case object percent extends ParameterUnit
  case object pages extends ParameterUnit
  case object times extends ParameterUnit
  case object lines extends ParameterUnit

  implicit val encoder : Encoder[ParameterUnit] = new Encoder[ParameterUnit] {
    def apply(a : ParameterUnit) = Json.string(a.toString)
  }

  implicit val decoder : Decoder[ParameterUnit] = new Decoder[ParameterUnit] {
    def apply(c: HCursor) = c.as[String] flatMap {
      case "milliseconds" => Right(milliseconds)
      case "seconds" => Right(seconds)
      case "minutes" => Right(minutes)
      case "hours" => Right(hours)
      case "bytes" => Right(bytes)
      case "kilobytes" => Right(kilobytes)
      case "megabytes" => Right(megabytes)
      case "gigabytes" => Right(gigabytes)
      case "percent" => Right(percent)
      case "pages" => Right(pages)
      case "times" => Right(times)
      case "lines" => Right(lines)
      case other => Left(DecodingFailure(s"Invalid unit $other", c.history))
    }
  }
}
