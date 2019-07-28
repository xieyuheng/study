package xieyuheng.cicada

import scala.collection.immutable.ListMap

import java.util.UUID

object eval {
  def eval(exp: Exp, ctx: Ctx): Either[ErrorMsg, Value] = {
    exp match {
      case Var(name) =>
        ctx.declEnv.get(name) match {
          case Some(DefineValue(name, value)) =>
            Right(value)
          case Some(DefineClass(name, map)) =>
            ???
          case Some(DefineUnion(name, map, subNames)) =>
            ???
          case None =>
            Right(NeutralValue(VarNeutral(name)))
        }
      case Type() =>
        Right(TypeValue(UUID.randomUUID().toString))
      case Union(name, map, subNames) =>
        for {
          map <- evalMap(map, ctx)
          id = UUID.randomUUID().toString
        } yield ??? // UnionValue(id, name, map, subNames)
      case Case(target, map) =>
        ???
      case Record(name, map) =>
        for {
          map <- evalMap(map, ctx)
          id = UUID.randomUUID().toString
        } yield ??? // RecordValue(id, name, map)
      case Field(target, fieldName) =>
        ???
      case Pi(args, ret) =>
        for {
          args <- evalMap(args, ctx)
          ret <- eval(ret, ctx)
        } yield PiValue(args, ret)
      case Fn(args, ret, body) =>
        for {
          args <- evalMap(args, ctx)
          ret <- eval(ret, ctx)
        } yield FnValue(args, ret, body, ctx)
      case Apply(target, args) =>
        ???
    }
  }

  def evalMap(
    map: ListMap[String, Exp],
    ctx: Ctx,
  ): Either[ErrorMsg, ListMap[String, Value]] = {
    val initResult: Either[ErrorMsg, ListMap[String, Value]] = Right(ListMap())
    ???
  }

}
