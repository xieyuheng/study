package xieyuheng.tartlet

case object Atom extends Type {
  def eval(env: Env): Either[Err, Val] =
    Right(ValAtom)

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case Atom => true
      case _ => false
    }
  }

  /*
   -----------------
   ctx :- Atom => Universe
   */
  def infer(ctx: Ctx): Either[Err, The] =
    Right(The(Universe, Atom))
}
