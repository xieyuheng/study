package xieyuheng.cicada

import collection.immutable.ListMap

object eval {

  def eval(env: Env, exp: Exp): Either[Err, Val] = {
    exp match {
      case Var(name: String) =>
        Right(NeuVar(name))

      case Type() =>
        Right(ValType())

      case Pi(arg_name: String, arg_type: Exp, ret_type: Exp) =>
        Right(ValPi(arg_name: String, arg_type: Exp, ret_type: Exp, env: Env))

      case Fn(arg_name: String, arg_type: Exp, body: Exp) =>
        Right(ValFn(arg_name: String, arg_type: Exp, body: Exp, env: Env))

      case Ap(target: Exp, arg: Exp) =>
        for {
          value <- eval(env, target)
          arg <- eval(env, arg)
          result <- val_ap(value, arg)
        } yield result

      case Cl(type_map: ListMap[String, Exp]) =>
        Right(ValCl(type_map: ListMap[String, Exp], env: Env))

      case Obj(val_map: ListMap[String, Exp]) =>
        val init: Either[Err, ListMap[String, Val]] = Right(ListMap.empty)
        val result = val_map.foldLeft(init) {
          case (result, (name, exp)) =>
            result match {
              case Right(map) => eval(env, exp) match {
                case Right(value) => Right(map ++ List((name, value)))
                case Left(err) => Left(err)
              }
              case Left(err) => Left(err)
            }
        }
        result match {
          case Right(map) => Right(ValObj(map))
          case Left(err) => Left(err)
        }

      case Dot(target: Exp, field: String) =>
        for {
          value <- eval(env, target)
          result <- val_dot(value, field)
        } yield result
    }
  }

  def val_ap(value: Val, arg: Val): Either[Err, Val] = {
    value match {
      case neu: Neu => Right(NeuAp(neu, arg))
      case ValFn(arg_name: String, arg_type: Exp, body: Exp, env: Env) =>
        for {
          arg_type <- eval(env, arg_type)
          result <- eval(env.ext(arg_name, arg_type), body)
        } yield result
      case _ => Left(Err("val_ap fail"))
    }
  }

  def val_dot(value: Val, field: String): Either[Err, Val] = {
    value match {
      case neu: Neu => Right(NeuDot(neu, field))
      case ValObj(val_map: ListMap[String, Val]) =>
        val_map.get(field) match {
          case Some(value) => Right(value)
          case None => Left(Err(s"missing field: ${field}"))
        }
      case _ => Left(Err("val_dot fail"))
    }
  }

}
