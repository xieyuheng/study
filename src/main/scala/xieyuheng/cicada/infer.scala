package xieyuheng.cicada

import collection.immutable.ListMap

import eval._
import subtype._
import readback._

object infer {

  def infer(ctx: Ctx, exp: Exp): Either[Err, Val] = {
    exp match {
      case Var(name: String) =>
        ???
      case Type() =>
        Right(ValType())
      case Pi(arg_map: ListMap[String, Exp], return_type: Exp) =>
        Right(ValType())
      case Fn(arg_map: ListMap[String, Exp], body: Exp) =>
        for {
          arg_value_map <- util.list_map_map_maybe_err(arg_map) {
            case (_name, exp) => eval(Env(), exp)
          }
          ctx2 = ctx.ext_map(arg_value_map)
          return_type_value <- infer(ctx2, body)
          return_type <- readback(ctx2, return_type_value)
        }  yield ValPi(arg_map, return_type, Env())
      case Cl(type_map: ListMap[String, Exp]) =>
        Right(ValType())
      case Obj(value_map: ListMap[String, Exp]) =>
        ???
      case Ap(target: Exp, arg_list: List[Exp]) =>
        // TODO after we infer target to a ValPi
        //   the main use of telescope will occur
        ???
      case Dot(target: Exp, field: String) =>
        // TODO after we infer target to a ValCl
        //   we need to readback the field in ctx extended by previous fields
        ???
      case Block(let_map: ListMap[String, Exp], body: Exp) =>
        ???
    }
  }

}
