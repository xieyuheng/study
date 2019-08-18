package xieyuheng.cicada

import scala.collection.immutable.ListMap

object eval {

  def apply(exp: Exp, env: Env): Either[ErrorMsg, Value] = {
    exp match {
      case Var(name) => {
        env.get(name) match {
          case Some(DefineValue(name, value)) =>
            Right(value)
          case Some(DefineRecord(name, map)) =>
            for {
              result <- evalMapToBind(map, env)
              (map, bind) = result
            } yield RecordValue(util.newId(), name, map, bind)
          case Some(DefineUnion(name, map, subNames)) =>
            for {
              result <- evalMapToBind(map, env)
              (map, bind) = result
            } yield UnionValue(util.newId(), name, map, subNames, bind)
          case None =>
            Right(NeutralValue(VarNeutral(name)))
        }
      }

      case Type() => {
        Right(TypeValue(util.newId()))
      }

      case Case(target, map) => {
        ???
      }

      case Field(target, fieldName) => {
        for {
          targetValue <- eval(target, env)
          result <- targetValue match {
            case union: UnionValue =>
              union.map.get(fieldName) match {
                case Some(value) => Right(value)
                case None => Left(ErrorMsg(s"no field: ${fieldName}, on union: ${union}"))
              }
            case record: RecordValue =>
              record.map.get(fieldName) match {
                case Some(value) => Right(value)
                case None => Left(ErrorMsg(s"no field: ${fieldName}, on record: ${record}"))
              }
            case NeutralValue(neutral) =>
              Right(NeutralValue(FieldNeutral(neutral, fieldName)))
            case _ =>
              Left(ErrorMsg(
                s"targetValue should be a UnionValue or RecordValue, instead of: ${targetValue}"))
          }
        } yield result
      }

      case Pi(args, ret) => {
        for {
          args <- evalMap(args, env)
          ret <- eval(ret, env)
        } yield PiValue(args, ret)
      }

      case Fn(args, ret, body) => {
        for {
          args <- evalMap(args, env)
          ret <- eval(ret, env)
        } yield FnValue(args, ret, body, env)
      }

      case Apply(target, args) => {
        ???
      }
    }
  }

  def evalMap(
    map: ListMap[String, Exp],
    env: Env,
  ): Either[ErrorMsg, ListMap[String, Value]] = {
    val initResult: Either[ErrorMsg, ListMap[String, Value]] = Right(ListMap())
    ???
  }

  def evalMapToBind(
    map: ListMap[String, Exp],
    env: Env,
  ): Either[ErrorMsg, (ListMap[String, Value], Bind)] = {
    val initResult: Either[ErrorMsg, ListMap[String, Value]] = Right(ListMap())
    ???
  }
}
