package xieyuheng.systemT

case class The (
  t: Type,
  exp: Exp,
) extends Eliminator {
  def eval(env: Env): Either[ErrorMsg, Value] = {
    exp.eval(env)
  }

  /*
   ctx :- e <= T
   -----------------
   ctx :- e: T => T
   */
  def infer(ctx: Ctx): Either[ErrorMsg, Type] = {
    Right(t)
  }
}
