package xieyuheng.cicada

import collection.immutable.ListMap

object eval {

  def eval(env: Env, exp: Exp): Either[Err, Val] = {
    exp match {
      case Var(name: String) =>
        env.lookup_val(name) match {
          case Some(value) => Right(value)
          case None => Right(NeuVar(name))
        }

      case Type() =>
        Right(ValType())

      case Pi(arg_map: ListMap[String, Exp], ret_type: Exp) =>
        Right(ValPi(arg_map: ListMap[String, Exp], ret_type: Exp, env: Env))

      case Fn(arg_map: ListMap[String, Exp], body: Exp) =>
        Right(ValFn(arg_map: ListMap[String, Exp], body: Exp, env: Env))

      case Ap(target: Exp, arg_list: List[Exp]) =>
        for {
          value <- eval(env, target)
          arg_list <- util.list_map_maybe_err(arg_list) { eval(env, _) }
          result <- val_ap(value, arg_list)
        } yield result

      case Cl(type_map: ListMap[String, Exp]) =>
        Right(ValCl(type_map: ListMap[String, Exp], env: Env))

      case Obj(val_map: ListMap[String, Exp]) =>
        for {
          val_map <- util.list_map_map_maybe_err(val_map) {
            case (_name, exp) => eval(env, exp)
          }
        } yield ValObj(val_map)

      case Dot(target: Exp, field: String) =>
        for {
          value <- eval(env, target)
          result <- val_dot(value, field)
        } yield result

      case Block(let_map: ListMap[String, Exp], body: Exp) =>
        var local_env = env
        for {
          _ <- util.list_map_map_maybe_err(let_map) {
            case (name, exp) =>
              eval(local_env, exp).map {
                case value =>
                  local_env = local_env.ext(name, value)
                  value
              }
          }
          result <- eval(local_env, body)
        } yield result
    }
  }

  def val_ap(value: Val, arg_list: List[Val]): Either[Err, Val] = {
    value match {
      case neu: Neu => Right(NeuAp(neu, arg_list))
      case ValFn(arg_map: ListMap[String, Exp], body: Exp, env: Env) =>
        val name_list = arg_map.keys.toList
        if (name_list.length != arg_map.size) {
          Left(Err("val_ap fail, ValFn arity mismatch"))
        } else {
          val map = Map(name_list.zip(arg_list): _*)
          eval(env.ext_map(map), body)
        }
      case ValCl(type_map: ListMap[String, Exp], env: Env) =>
        val name_list = type_map.keys.toList
        if (name_list.length != type_map.size) {
          Left(Err("val_ap fail, ValCl arity mismatch"))
        } else {
          val val_map = ListMap(name_list.zip(arg_list): _*)
          Right(ValObj(val_map))
        }
      case _ => Left(Err(
        "val_ap fail, expecting ValFn or ValCl\n" +
          s"value: ${value}\n"
      ))
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
      case _ => Left(Err("val_dot fail, expecting ValObj"))
    }
  }

}
