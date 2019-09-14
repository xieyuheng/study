package xieyuheng.systemt

case class The (
  t: Type,
  exp: Exp,
) extends Eliminator {
  def eval(env: Env): Either[Err, Val] = {
    exp.eval(env)
  }

  /*
   ctx :- e <= T
   -----------------
   ctx :- e: T => T
   */
  def infer(ctx: Ctx): Either[Err, Type] = {
    Right(t)
  }
}
