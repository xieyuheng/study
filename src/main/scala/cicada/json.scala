package cicada

import cicada.dsl._

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
    implicit def rwCase: RW[Case] = macroRW
    implicit def rwField: RW[Field] = macroRW
    implicit def rwPi: RW[Pi] = macroRW
    implicit def rwFn: RW[Fn] = macroRW
    implicit def rwAp: RW[Ap] = macroRW
    implicit def rwExp: RW[Exp] = RW.merge(
      rwVar,
      rwType,
      rwThe,
      rwCase,
      rwField,
      rwPi,
      rwFn,
      rwAp,
    )

    implicit def rwId: RW[Id] = macroRW

    implicit def rwBind: RW[Bind] =
      readwriter[Map[Id, Value]]
        .bimap[Bind](_.map, Bind(_))

    implicit def rwEnv: RW[Env] =
      readwriter[Map[String, Def]]
        .bimap[Env](_.map, Env(_))

    // Def
    implicit def rwDefineValue: RW[DefineValue] = macroRW
    implicit def rwDefineMemberType: RW[DefineMemberType] = macroRW
    implicit def rwDefineSumType: RW[DefineSumType] = macroRW
    implicit def rwDefineFn: RW[DefineFn] = macroRW
    implicit def rwDef: RW[Def] = RW.merge(
      rwDefineValue,
      rwDefineMemberType,
      rwDefineSumType,
      rwDefineFn,
    )

    // Value
    implicit def rwTypeOfType: RW[TypeOfType] = macroRW
    implicit def rwValueOfType: RW[ValueOfType] = macroRW
    implicit def rwSumTypeValue: RW[SumTypeValue] = macroRW
    implicit def rwMemberTypeValue: RW[MemberTypeValue] = macroRW
    implicit def rwPiValue: RW[PiValue] = macroRW
    implicit def rwFnValue: RW[FnValue] = macroRW
    implicit def rwNeutralValue: RW[NeutralValue] = macroRW
    implicit def rwTopValue: RW[TopValue] = macroRW
    implicit def rwBottomValue: RW[BottomValue] = macroRW
    implicit def rwValue: RW[Value] = RW.merge(
      rwTypeOfType,
      rwValueOfType,
      rwSumTypeValue,
      rwMemberTypeValue,
      rwPiValue,
      rwFnValue,
      rwNeutralValue,
      rwTopValue,
      rwBottomValue,
    )

    // Neutral
    implicit def rwVarNeutral: RW[VarNeutral] = macroRW
    implicit def rwCaseNeutral: RW[CaseNeutral] = macroRW
    implicit def rwFieldNeutral: RW[FieldNeutral] = macroRW
    implicit def rwApNeutral: RW[ApNeutral] = macroRW
    implicit def rwNeutral: RW[Neutral] = RW.merge(
      rwVarNeutral,
      rwCaseNeutral,
      rwFieldNeutral,
      rwApNeutral,
    )
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
