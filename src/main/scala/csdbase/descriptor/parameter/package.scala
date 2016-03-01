package csdbase.descriptor

import java.net.{URISyntaxException, URI}

import cats.data.Xor.{Left, Right}
import io.circe.{DecodingFailure, Decoder, Json, Encoder}


package object parameter {
  object EncoderInstances {
    implicit val URIEncoder = Encoder.instance[URI] {
      uri => Json.string(uri.toString)
    }
  }

  object DecoderInstances {
    implicit val URIDecoder = Decoder.instance[URI] {
      cursor => cursor.as[String] flatMap {
        str =>
          try
            Right(new URI(str))
          catch {
            case err: URISyntaxException =>
              Left(DecodingFailure(s"""Failed to parse "$str" as URI: ${err.getMessage}""", cursor.history))
          }
      }
    }
  }
}
