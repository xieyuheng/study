package xieyuheng.tartlet

case object Zero extends Constructor {
  def eval(env: Env): Either[Err, Val] =
    Right(ValZero)

  def alpha_eq(
    that: Exp,
    this_map: Map[String, String],
    that_map: Map[String, String],
  ): Boolean = {
    that match {
      case Zero => true
      case _ => false
    }
  }

  /*
   ---------------------
   ctx :- Zero <= Nat
   */
  def check(ctx: Ctx, t: Val): Either[Err, Exp] =
    t match {
      case ValNat =>
        Right(this)
      case _ =>
        Left(Err(
          s"expected ValNat, found: ${t}"))
    }
}
