package xieyuheng.cicada

import collection.immutable.ListMap

object infer {

  def infer(ctx: Ctx, exp: Exp): Either[Err, Val] = {
    exp match {
      case Var(name: String) =>
        ctx.lookup_type(name) match {
          case Some(s) => Right(s)
          case None => Left(Err(s"can not find var: ${name} in ctx"))
        }

      case Type() =>
        Right(ValType())

      case Pi(arg_map: ListMap[String, Exp], ret_type: Exp) =>
        Right(ValType())

      case Fn(arg_map: ListMap[String, Exp], body: Exp) =>
        for {
          ctx <- infer_ctx_from_telescope(ctx, arg_map)
          // TODO need to construct a Pi type here
          //   which means 
          result <- infer(ctx, body)
        } yield result

      case Ap(target: Exp, arg_list: List[Exp]) =>
        ???

      case Cl(type_map: ListMap[String, Exp]) =>
        Right(ValType())

      case Obj(val_map: ListMap[String, Exp]) =>
        ???

      case Dot(target: Exp, field: String) =>
        ???

      case Block(let_map: ListMap[String, Exp], body: Exp) =>
        ???
    }
  }

  def infer_ctx_from_telescope(
    ctx: Ctx, telescope: ListMap[String, Exp],
  ): Either[Err, Ctx] = {
    ???
  }

}
