package xieyuheng.systemt

case object Zero extends Constructor {
  def eval(env: Env): Either[Err, Value] = {
    Right(ValueZero)
  }

  /*
   -------------------
   ctx :- Zero <= Nat
   */
  def check(ctx: Ctx, t: Type): Either[Err, Unit] = {
    t match {
      case Nat => Right(())
      case _ =>
        Left(Err("the type of Zero should be Nat"))
    }
  }
}
