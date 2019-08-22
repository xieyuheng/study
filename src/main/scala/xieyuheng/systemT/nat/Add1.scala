package xieyuheng..systemT

case class Add1(prev: Exp) extends Constructor {
  def eval(env: Env): Either[ErrorMsg, Value] = {
    for {
      prevValue <- prev.eval(env)
    } yield ValueAdd1(prevValue)
  }

  /*
   ctx :- prev <= Nat
   -------------------
   ctx :- Add1 (prev) <= Nat
   */
  def check(ctx: Ctx, t: Type): Either[ErrorMsg, Unit] = {
    t match {
      case Nat =>
        prev.check(ctx, t)
      case _ =>
        Left(ErrorMsg("the type of Add1 should be Nat"))
    }
  }
}
