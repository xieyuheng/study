package xieyuheng.tartlet

case object Sole extends Constructor {
  def eval(env: Env): Either[Err, Val] =
    Right(ValSole)

  def alpha_eq(
    that: Exp,
    this_map: Map[String, String],
    that_map: Map[String, String],
  ): Boolean = {
    that match {
      case Sole => true
      case _ => false
    }
  }

  /*
   ---------------------
   ctx :- Sole <= Trivial
   */
  def check(ctx: Ctx, t: Val): Either[Err, Exp] =
    t match {
      case ValTrivial =>
        Right(this)
      case _ =>
        Left(Err(
          s"expected ValTrivial, found: ${t}"))
    }
}
