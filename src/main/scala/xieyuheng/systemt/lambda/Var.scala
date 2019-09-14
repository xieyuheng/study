package xieyuheng.systemt

case class Var (name: String) extends Eliminator {
  def eval(env: Env): Either[Err, Val] = {
    env.lookupVal(name) match {
      case Some(value) =>
        Right(value)
      case None =>
        Left(Err(s"can not find var: ${this} in env"))
    }
  }

  /*
   ctx.lookupType(x) == T
   --------------------------
   ctx :- Var(x) => T
   */
  def infer(ctx: Ctx): Either[Err, Type] = {
    ctx.lookupType(name) match {
      case Some(t) =>
        Right(t)
      case None =>
        Left(Err(s"can not find var: ${this} in ctx"))
    }
  }
}
