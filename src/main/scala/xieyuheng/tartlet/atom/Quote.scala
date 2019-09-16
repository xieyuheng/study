package xieyuheng.tartlet

case class Quote (sym: String) extends Constructor {
  def eval(env: Env): Either[Err, Val] =
    Right(ValQuote(sym))

  def alpha_eq(
    that: Exp,
    this_map: Map[String, String],
    that_map: Map[String, String],
  ): Boolean = {
    that match {
      case Quote(sym2) => sym == sym2
      case _ => false
    }
  }

  /*
   ---------------------
   ctx :- Quote(sym) <= Atom
   */
  def check(ctx: Ctx, t: Val): Either[Err, The] =
    t match {
      case ValAtom =>
        Right(
          The(Atom, this))
      case _ =>
        Left(Err(
          s"expected ValAtom, found: ${t}"))
    }
}
