package xieyuheng.tt.systemT

case object Zero extends Constructor {
  def eval(env: Env): Either[ErrorMsg, Value] = {
    Right(ValueZero)
  }

  /*
   -------------------
   ctx :- Zero <= Nat
   */
  def check(ctx: Ctx, t: Type): Either[ErrorMsg, Unit] = {
    t match {
      case Nat => Right(())
      case _ =>
        Left(ErrorMsg("the type of Zero should be Nat"))
    }
  }
}
