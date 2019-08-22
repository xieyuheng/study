package cicada

import upickle.default._
import upickle.default.{ ReadWriter => RW }

object json {
  object rw {
    implicit val rwId: RW[Id] = macroRW
    implicit val rwTypeOfType: RW[TypeOfType] = macroRW
  }
}

object jsonTest extends Test {
  import json.rw._

  // ujson.write: ujson.Value => String
  // ujson.read: String => ujson.Value

  println(ujson.write(ujson.Num(3.14)): String)

  // upickle.default.write[T]: T => String
  // upickle.default.read[T]: String => T

  println(write(TypeOfType(Id("x"))): String)
  println(read[TypeOfType](write(TypeOfType(Id("x")))): TypeOfType)

  // T can be ujson.Value

  println(write(ujson.Num(3.14)): String)
  println(read[ujson.Num](write(ujson.Num(3.14))): ujson.Num)

  // upickle.default.writeJs[T]: T => ujson.Value
  // upickle.default.read[T]: ujson.Value => T

  println(writeJs(TypeOfType(Id("x"))): ujson.Value)
  println(read[TypeOfType](writeJs(TypeOfType(Id("x")))): TypeOfType)
}
