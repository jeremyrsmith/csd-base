package csdbase.descriptor.parameter

import java.net.URI
import java.nio.file.Path

import csdbase.descriptor.parameter.Parameter.ParameterCoproduct
import io.circe._, io.circe.generic.auto._
import cats.data.Xor, Xor._

import EncoderInstances._
import DecoderInstances._
import shapeless._
import shapeless.ops.coproduct.Inject

sealed abstract class ParameterType[T : Encoder : Decoder](val decode : (HCursor => Decoder.Result[Parameter[T]])) {
  val TypeName = this.toString

  def decodeCP(cursor: HCursor) = decode(cursor) map (Coproduct[ParameterCoproduct](_))
}

trait EncodeHelper {
  def stringEncoder[T](value: T) : Encoder[T] = new Encoder[T] {
    def apply(t: T) = Json.string(t.toString)
  }
}

object ParameterType extends EncodeHelper{


  object types {
    case object boolean extends ParameterType[Boolean](_.as[BooleanParameter])
    case object double extends ParameterType[Double](_.as[DoubleParameter])
    case object long extends ParameterType[Long](_.as[LongParameter])
    case object memory extends ParameterType[Long](_.as[MemoryParameter])
    case object port extends ParameterType[Int](_.as[PortParameter])
    case object string_enum extends ParameterType[String](_.as[StringEnumParameter])
    case object string extends ParameterType[String](_.as[StringParameter])
    case object password extends ParameterType[String](_.as[PasswordParameter])
    case object string_array extends ParameterType[Seq[String]](_.as[StringArrayParameter])
    case object path extends ParameterType[String](_.as[PathParameter])
    case object path_array extends ParameterType[Seq[String]](_.as[PathArrayParameter])
    case object uri extends ParameterType[URI](_.as[UriParameter])
    case object uri_array extends ParameterType[Seq[URI]](_.as[UriArrayParameter])
  }
  
  import types._


  implicit val booleanEncoder : Encoder[boolean.type] = stringEncoder(boolean)
  implicit val doubleEncoder : Encoder[double.type] = stringEncoder(double)
  implicit val longEncoder : Encoder[long.type] = stringEncoder(long)
  implicit val memoryEncoder : Encoder[memory.type] = stringEncoder(memory)
  implicit val portEncoder : Encoder[port.type] = stringEncoder(port)
  implicit val string_enumEncoder : Encoder[string_enum.type] = stringEncoder(string_enum)
  implicit val stringEncoder : Encoder[string.type] = stringEncoder(string)
  implicit val passwordEncoder : Encoder[password.type] = stringEncoder(password)
  implicit val string_arrayEncoder : Encoder[string_array.type] = stringEncoder(string_array)
  implicit val pathEncoder : Encoder[path.type] = stringEncoder(path)
  implicit val path_arrayEncoder : Encoder[path_array.type] = stringEncoder(path_array)
  implicit val uriEncoder : Encoder[uri.type] = stringEncoder(uri)
  implicit val uri_arrayEncoder : Encoder[uri_array.type] = stringEncoder(uri_array)


}
