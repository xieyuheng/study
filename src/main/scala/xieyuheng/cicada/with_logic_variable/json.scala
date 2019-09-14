package xieyuheng.cicada.with_logic_variable

import xieyuheng.cicada.with_logic_variable.expDSL._

import upickle.default._
import upickle.default.{ ReadWriter => RW }

import scala.collection.immutable.ListMap

object json {
  object rw {
    implicit def rwMultiMap[K, V]
      (implicit krw: RW[K], vrw: RW[V])
        : RW[MultiMap[K, V]] =
      readwriter[List[(K, V)]]
        .bimap[MultiMap[K, V]](_.entries, MultiMap(_))

    implicit def rwListMap[K, V]
      (implicit krw: RW[K], vrw: RW[V])
        : RW[ListMap[K, V]] =
      readwriter[Map[K, V]]
        .bimap[ListMap[K, V]](_.toMap, map => ListMap(map.toList: _*))

    implicit def rwDic[V]
      (implicit vrw: RW[V])
        : RW[Map[String, V]] =
      readwriter[ujson.Value]
        .bimap[Map[String, V]](
          map => ujson.Obj.from(map.mapValues(writeJs[V](_))),
          json => scala.collection.immutable.Map() ++
            json.obj.value.mapValues(read[V](_)))

    implicit def rwListDic[V]
      (implicit vrw: RW[V])
        : RW[ListMap[String, V]] =
      readwriter[Map[String, V]]
        .bimap[ListMap[String, V]](_.toMap, map => ListMap(map.toSeq: _*))

    // Exp
    implicit def rwVar: RW[Var] = macroRW
    implicit def rwType: RW[Type] = macroRW
    implicit def rwThe: RW[The] = macroRW
    implicit def rwChoice: RW[Choice] = macroRW
    implicit def rwDot: RW[Dot] = macroRW
    implicit def rwPi: RW[Pi] = macroRW
    implicit def rwFn: RW[Fn] = macroRW
    implicit def rwAp: RW[Ap] = macroRW
    implicit def rwExp: RW[Exp] = RW.merge(
      rwVar,
      rwType,
      rwThe,
      rwChoice,
      rwDot,
      rwPi,
      rwFn,
      rwAp,
    )

    implicit def rwId: RW[Id] = macroRW

    implicit def rwBind: RW[Bind] =
      readwriter[Map[Id, Val]]
        .bimap[Bind](_.map, Bind(_))

    implicit def rwEnv: RW[Env] =
      readwriter[Map[String, Define]]
        .bimap[Env](_.map, Env(_))

    // Define
    implicit def rwDefineVal: RW[DefineVal] = macroRW
    implicit def rwDefineMemberType: RW[DefineMemberType] = macroRW
    implicit def rwDefineSumType: RW[DefineSumType] = macroRW
    implicit def rwDefineFn: RW[DefineFn] = macroRW
    implicit def rwDefine: RW[Define] = RW.merge(
      rwDefineVal,
      rwDefineMemberType,
      rwDefineSumType,
      rwDefineFn,
    )

    // Val
    implicit def rwTypeOfType: RW[TypeOfType] = macroRW
    implicit def rwValOfType: RW[ValOfType] = macroRW
    implicit def rwSumTypeVal: RW[SumTypeVal] = macroRW
    implicit def rwMemberTypeVal: RW[MemberTypeVal] = macroRW
    implicit def rwPiVal: RW[PiVal] = macroRW
    implicit def rwFnVal: RW[FnVal] = macroRW
    implicit def rwNeuVal: RW[NeuVal] = macroRW
    implicit def rwTopVal: RW[TopVal] = macroRW
    implicit def rwBottomVal: RW[BottomVal] = macroRW
    implicit def rwVal: RW[Val] = RW.merge(
      rwTypeOfType,
      rwValOfType,
      rwSumTypeVal,
      rwMemberTypeVal,
      rwPiVal,
      rwFnVal,
      rwNeuVal,
      rwTopVal,
      rwBottomVal,
    )

    // Neu
    implicit def rwVarNeu: RW[VarNeu] = macroRW
    implicit def rwChoiceNeu: RW[ChoiceNeu] = macroRW
    implicit def rwDotNeu: RW[DotNeu] = macroRW
    implicit def rwApNeu: RW[ApNeu] = macroRW
    implicit def rwNeu: RW[Neu] = RW.merge(
      rwVarNeu,
      rwChoiceNeu,
      rwDotNeu,
      rwApNeu,
    )
  }
}

object jsonTest extends App {
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
    ("cons_t" ap %(
      "A" -> "nat_t",
      "head" -> "zero_t",
      "tail" -> ("cons_t" ap %(
        "A" -> "nat_t",
        "head" -> "zero_t",
        "tail" -> ("cons_t" ap %(
          "A" -> "nat_t",
          "head" -> "zero_t",
          "tail" -> "null_t"))))), indent = 2))
}
