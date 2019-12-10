package xieyuheng.cicada

import collection.immutable.ListMap

import eval._
import subtype._
import readback._

object infer {

  def infer(env: Env, ctx: Ctx, exp: Exp): Either[Err, Val] = {
    exp match {
      case Var(name: String) =>
        ctx.lookup_type(name) match {
          case Some(t) => Right(t)
          case None => Left(Err(s"can not find var: ${name} in ctx"))
        }

      case Type() =>
        Right(ValType())

      case Pi(arg_map: ListMap[String, Exp], return_type: Exp) =>
        Right(ValType())

      case Fn(arg_map: ListMap[String, Exp], body: Exp) =>
        var local_ctx = ctx
        for {
          _ <- util.list_map_map_maybe_err(arg_map) {
            case (name, exp) => eval(env, exp).map {
              case value =>
                local_ctx = local_ctx.ext(name, value)
            }
          }
          return_type_value <- infer(env, local_ctx, body)
          return_type <- readback(local_ctx, return_type_value)
        }  yield ValPi(arg_map, return_type, env)

      case Cl(type_map: ListMap[String, Exp]) =>
        Right(ValType())

      case Obj(value_map: ListMap[String, Exp]) =>
        for {
          value_map <- util.list_map_map_maybe_err(value_map) {
            case (name, exp) =>
              eval(env, exp).flatMap { readback(ctx, _) }
          }
        }  yield ValCl(value_map, env)

      case Ap(target: Exp, arg_list: List[Exp]) =>
        // TODO after we infer target to a ValPi
        //   the main use of telescope will occur
        ???

      case Dot(target: Exp, field: String) =>
        // TODO after we infer target to a ValCl
        //   we need to readback the field in ctx extended by previous fields
        ???

      case Block(let_map: ListMap[String, Exp], body: Exp) =>
        var local_ctx = ctx
        for {
          _ <- util.list_map_map_maybe_err(let_map) {
            case (name, exp) => eval(env, exp).map {
              case value =>
                local_ctx = local_ctx.ext(name, value)
            }
          }
          result <- infer(env, local_ctx, body)
        }  yield result
    }
  }

}
