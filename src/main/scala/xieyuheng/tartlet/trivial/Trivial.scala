package xieyuheng.tartlet

case object Trivial extends Type {
  def eval(env: Env): Either[ErrorMsg, Value] =
    Right(ValueTrivial)

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case Trivial => true
      case _ => false
    }
  }

  /*
   -----------------
   ctx :- Trivial => Universe
   */
  def infer(ctx: Ctx): Either[ErrorMsg, The] =
    Right(The(Universe, Trivial))
}
