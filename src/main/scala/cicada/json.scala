package cicada

import cicada.dsl._

import upickle.default._
import upickle.default.{ ReadWriter => RW }

import scala.collection.immutable.ListMap

object json {
  object rw {
    implicit def rwMultiMap[K, V]
      (implicit krw:RW[K], vrw:RW[V])
        : RW[MultiMap[K, V]] =
      readwriter[List[(K, V)]]
        .bimap[MultiMap[K, V]](
          _.entries,
          MultiMap(_))

    implicit def rwListMap[K, V]
      (implicit krw:RW[K], vrw:RW[V])
        : RW[ListMap[K, V]] =
      readwriter[Map[K, V]]
        .bimap[ListMap[K, V]](
          _.toMap,
          map => ListMap(map.toList: _*))

    // Exp
    implicit def rwVar: RW[Var] = macroRW
    implicit def rwType: RW[Type] = macroRW
    implicit def rwThe: RW[The] = macroRW
    implicit def rwCase: RW[Case] = macroRW
    implicit def rwField: RW[Field] = macroRW
    implicit def rwPi: RW[Pi] = macroRW
    implicit def rwFn: RW[Fn] = macroRW
    implicit def rwAp: RW[Ap] = macroRW
    implicit def rwExp: RW[Exp] = RW.merge(
      rwVar, rwType, rwThe, rwCase, rwField, rwPi, rwFn, rwAp)

    // Value
    implicit def rwId: RW[Id] = macroRW
    implicit def rwTypeOfType: RW[TypeOfType] = macroRW
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

  // Exp
  println(write[Exp]
    ("cons_t" ap $(
      "A" -> "nat_t",
      "head" -> "zero_t",
      "tail" -> ("cons_t" ap $(
        "A" -> "nat_t",
        "head" -> "zero_t",
        "tail" -> ("cons_t" ap $(
          "A" -> "nat_t",
          "head" -> "zero_t",
          "tail" -> "null_t"))))), indent = 2))
}
