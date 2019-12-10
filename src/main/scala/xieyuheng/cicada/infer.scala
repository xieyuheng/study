package xieyuheng.cicada

import collection.immutable.ListMap

import eval._
import subtype._
import readback._

object infer {

  def infer(ctx: Ctx, value: Val): Either[Err, Val] = {
    value match {
      case ValType() =>
        Right(ValType())
      case ValPi(arg_map: ListMap[String, Exp], return_type: Exp, env: Env) =>
        Right(ValType())
      case ValFn(arg_map: ListMap[String, Exp], body: Exp, env: Env) =>
        for {
          return_value <- eval(env, body)
          arg_value_map <- util.list_map_map_maybe_err(arg_map) {
            case (_name, exp) => eval(env, exp)
          }
          return_type <- infer(ctx.ext_map(arg_value_map), return_value)
          return_type_exp <- readback(ctx.ext_map(arg_value_map), return_type)
        } yield ValPi(arg_map, return_type_exp, env)
      case ValCl(type_map: ListMap[String, Exp], env: Env) =>
        Right(ValType())
      case ValObj(value_map: ListMap[String, Val]) =>
        ???
      case NeuVar(name: String) =>
        ???
      case NeuAp(target: Neu, arg_list: List[Val]) =>
        // TODO after we infer target to a ValPi
        //   the main use of telescope will occur
        ???
      case NeuDot(target: Neu, field: String) =>
        // TODO after we infer target to a ValCl
        //   we need to readback the field in ctx extended by previous fields
        ???
    }
  }

}
