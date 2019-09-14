package xieyuheng.systemt

case class Add1(prev: Exp) extends Constructor {
  def eval(env: Env): Either[Err, Value] = {
    for {
      prevValue <- prev.eval(env)
    } yield ValueAdd1(prevValue)
  }

  /*
   ctx :- prev <= Nat
   -------------------
   ctx :- Add1 (prev) <= Nat
   */
  def check(ctx: Ctx, t: Type): Either[Err, Unit] = {
    t match {
      case Nat =>
        prev.check(ctx, t)
      case _ =>
        Left(Err("the type of Add1 should be Nat"))
    }
  }
}
