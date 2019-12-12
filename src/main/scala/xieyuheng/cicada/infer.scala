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
          type_map <- util.list_map_map_maybe_err(value_map) {
            case (name, exp) =>
              eval(env, exp).flatMap { readback(ctx, _) }
          }
        }  yield ValCl(type_map, env)

      case Ap(target: Exp, arg_list: List[Exp]) =>
        infer(env, ctx, target) match {
          case Right(ValPi(arg_map: ListMap[String, Exp], return_type: Exp, env: Env)) =>
            // NOTE the main use of telescope will occur
            ???
          case Right(t) =>
            Left(Err(s"expecting pi type but found: ${t}"))
          case Left(err) =>
            Left(err)
        }

      case Dot(target: Exp, field: String) =>
        infer(env, ctx, target) match {
          case Right(ValCl(type_map: ListMap[String, Exp], env: Env)) =>
            // NOTE we need to readback the field value
            //   in ctx extended by previous fields
            ???
          case Right(t) =>
            Left(Err(s"expecting class type but found: ${t}"))
          case Left(err) =>
            Left(err)
        }

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
