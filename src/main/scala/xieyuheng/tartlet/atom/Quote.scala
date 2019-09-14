package xieyuheng.tartlet

case class Quote (sym: String) extends Constructor {
  def eval(env: Env): Either[Err, Value] =
    Right(ValueQuote(sym))

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
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
  def check(ctx: Ctx, t: Value): Either[Err, The] =
    t match {
      case ValueAtom =>
        Right(
          The(Atom, this))
      case _ =>
        Left(Err(
          s"expected ValueAtom, found: ${t}"))
    }
}
