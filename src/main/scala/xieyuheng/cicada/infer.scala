package xieyuheng.cicada

import collection.immutable.ListMap

object infer {

  def infer(ctx: Ctx, exp: Exp): Either[Err, Exp] = {
    exp match {
      case Var(name: String) =>
        ctx.lookup_type(name) match {
          case Some(s) => Right(s)
          case None => Left(Err(s"can not find var: ${name} in ctx"))
        }

      case Type() =>
        Right(Type())

      case Pi(arg_map: ListMap[String, Exp], ret_type: Exp) =>
        Right(Type())

      case Fn(arg_map: ListMap[String, Exp], body: Exp) =>
        for {
          ret_type <- infer(ctx.ext_map(arg_map), body)
        } yield Pi(arg_map, ret_type)

      case Ap(target: Exp, arg_list: List[Exp]) =>
        ???

      case Cl(type_map: ListMap[String, Exp]) =>
        Right(Type())

      case Obj(val_map: ListMap[String, Exp]) =>
        ???

      case Dot(target: Exp, field: String) =>
        ???

      case Block(let_map: ListMap[String, Exp], body: Exp) =>
        ???
    }
  }

}
