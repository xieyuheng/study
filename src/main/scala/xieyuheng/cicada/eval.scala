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
          arg_list <- list_eval(env, arg_list)
          result <- val_ap(value, arg_list)
        } yield result

      case Cl(type_map: ListMap[String, Exp]) =>
        Right(ValCl(type_map: ListMap[String, Exp], env: Env))

      case Obj(val_map: ListMap[String, Exp]) =>
        list_map_eval(env, val_map) match {
          case Right(map) => Right(ValObj(map))
          case Left(err) => Left(err)
        }

      case Dot(target: Exp, field: String) =>
        for {
          value <- eval(env, target)
          result <- val_dot(value, field)
        } yield result

      case Block(let_map: ListMap[String, Exp], body: Exp) =>
        var local_env = env
        val init: Either[Err, ListMap[String, Val]] = Right(ListMap.empty)
        val result = let_map.foldLeft(init) {
          case (result, (name, exp)) =>
            result match {
              case Right(map) => eval(local_env, exp) match {
                case Right(value) =>
                  local_env = local_env.ext(name, value)
                  Right(map ++ List((name, value)))
                case Left(err) => Left(err)
              }
              case Left(err) => Left(err)
            }
        }
        result match {
          case Right(map) => eval(local_env, body)
          case Left(err) => Left(err)
        }
    }
  }

  def list_map_eval(env: Env, list_map: ListMap[String, Exp]): Either[Err, ListMap[String, Val]] = {
    val init: Either[Err, ListMap[String, Val]] = Right(ListMap.empty)
    list_map.foldLeft(init) {
      case (result, (name, exp)) =>
        result match {
          case Right(map) => eval(env, exp) match {
            case Right(value) => Right(map ++ List((name, value)))
            case Left(err) => Left(err)
          }
          case Left(err) => Left(err)
        }
    }
  }

  def list_eval(env: Env, list: List[Exp]): Either[Err, List[Val]] = {
    val init: Either[Err, List[Val]] = Right(List.empty)
    list.foldLeft(init) {
      case (result, exp) =>
        result match {
          case Right(map) => eval(env, exp) match {
            case Right(value) => Right(map :+ value)
            case Left(err) => Left(err)
          }
          case Left(err) => Left(err)
        }
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
