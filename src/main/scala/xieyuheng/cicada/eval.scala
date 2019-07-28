package xieyuheng.cicada

import scala.collection.immutable.ListMap

import java.util.UUID

object eval {
  def eval(exp: Exp, env: Map[String, Def]): Either[ErrorMsg, Value] = {
    exp match {
      case Var(name) =>
        env.get(name) match {
          case Some(DefineValue(name, value)) =>
            Right(value)
          case Some(DefineRecord(name, map)) =>
            for {
              result <- evalMapWithBind(map, env)
              (map, bind) = result
              id = UUID.randomUUID().toString
            } yield RecordValue(id, name, map, bind)
          case Some(DefineUnion(name, map, subNames)) =>
            for {
              result <- evalMapWithBind(map, env)
              (map, bind) = result
              id = UUID.randomUUID().toString
            } yield UnionValue(id, name, map, subNames, bind)
          case None =>
            Right(NeutralValue(VarNeutral(name)))
        }
      case Type() =>
        Right(TypeValue(UUID.randomUUID().toString))
      case Case(target, map) =>
        ???
      case Field(target, fieldName) =>
        ???
      case Pi(args, ret) =>
        for {
          args <- evalMap(args, env)
          ret <- eval(ret, env)
        } yield PiValue(args, ret)
      case Fn(args, ret, body) =>
        for {
          args <- evalMap(args, env)
          ret <- eval(ret, env)
        } yield FnValue(args, ret, body, env)
      case Apply(target, args) =>
        ???
    }
  }

  def evalMap(
    map: ListMap[String, Exp],
    env: Map[String, Def],
  ): Either[ErrorMsg, ListMap[String, Value]] = {
    val initResult: Either[ErrorMsg, ListMap[String, Value]] = Right(ListMap())
    ???
  }

  def evalMapWithBind(
    map: ListMap[String, Exp],
    env: Map[String, Def],
  ): Either[ErrorMsg, (ListMap[String, Value], Ctx.Bind)] = {
    val initResult: Either[ErrorMsg, ListMap[String, Value]] = Right(ListMap())
    ???
  }

}
