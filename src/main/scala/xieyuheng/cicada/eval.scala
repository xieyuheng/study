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
            } yield RecordValue(name, map, bind)
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
        Right(LogicVar(util.newId()))
      }

      case Case(target, map) => {
        for {
          targetValue <- eval(target, env)
          result <- targetValue match {
            case record: RecordValue =>
              map.get(record.name) match {
                case Some(exp) => eval(exp, env)
                case None => Left(ErrorMsg(s"no clause: ${record.name}, on case: ${map}"))
              }
            case NeutralValue(neutral) =>
              for {
                map <- evalMap(map, env)
              } yield NeutralValue(CaseNeutral(neutral, map))
            case _ =>
              Left(ErrorMsg("targetValue of Field should be RecordValue or NeutralValue, " +
                s"instead of: ${targetValue}"))
          }
        } yield result
      }

      case Field(target, fieldName) => {
        for {
          targetValue <- eval(target, env)
          result <- targetValue match {
            case union: UnionValue =>
              union.map.get(fieldName) match {
                case Some(value) => Right(util.deepWalk(value, union.bind))
                case None => Left(ErrorMsg(s"no field: ${fieldName}, on union: ${union}"))
              }
            case record: RecordValue =>
              record.map.get(fieldName) match {
                case Some(value) => Right(util.deepWalk(value, record.bind))
                case None => Left(ErrorMsg(s"no field: ${fieldName}, on record: ${record}"))
              }
            case NeutralValue(neutral) =>
              Right(NeutralValue(FieldNeutral(neutral, fieldName)))
            case _ =>
              Left(ErrorMsg("targetValue of Field should be UnionValue, RecordValue or NeutralValue, " +
                s"instead of: ${targetValue}"))
          }
        } yield result
      }

      case Pi(args, ret) => {
        for {
          args <- evalMap(args, env)
          ret <- eval(ret, env)
        } yield PiValue(util.newId(), args, ret)
      }

      case Fn(args, ret, body) => {
        for {
          args <- evalMap(args, env)
          ret <- eval(ret, env)
        } yield FnValue(args, ret, body, env)
      }

      case Apply(target, args) => {
        for {
          targetValue <- eval(target, env)
          result <- targetValue match {
            case _ =>
              Left(ErrorMsg(""))
          }
        } yield result
      }
    }
  }

  def evalMap(
    map: ListMap[String, Exp],
    env: Env,
  ): Either[ErrorMsg, ListMap[String, Value]] = {
    val initResult: Either[ErrorMsg, ListMap[String, Value]] = Right(ListMap())
    map.foldLeft(initResult) { case (result, (name, exp)) =>
      for {
        map <- result
        value <- eval(exp, env)
      } yield map + (name -> value)
    }
  }

  def evalMapToBind(
    map: ListMap[String, Exp],
    env: Env,
  ): Either[ErrorMsg, (ListMap[String, Value], Bind)] = {
    val initResult: Either[ErrorMsg, ListMap[String, Value]] = Right(ListMap())
    ???
  }
}
