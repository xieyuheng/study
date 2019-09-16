package xieyuheng.tartlet

case object Same extends Constructor {
  def eval(env: Env): Either[Err, Val] = {
    Right(ValSame)
  }

  def alpha_eq(
    that: Exp,
    this_map: Map[String, String],
    that_map: Map[String, String],
  ): Boolean = {
    that match {
      case Same => true
      case _ => false
    }
  }

  /*
    ctx :- conversion_check(T, from, to)
    ---------------------
    ctx :- Same <= Eqv(T, from, to)
   */
  def check(ctx: Ctx, t: Val): Either[Err, Exp] = {
    t match {
      case ValEqv(typeVal, from, to) =>
        for {
          _ok <- util.conversion_check(ctx, typeVal, from, to)
        } yield this
      case _ =>
        Left(Err(
          s"expected ValEqv(typeVal, from, to), found: ${t}"))
    }
  }
}
