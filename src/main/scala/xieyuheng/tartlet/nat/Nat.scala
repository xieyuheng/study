package xieyuheng.tartlet

case object Nat extends Type {
  def eval(env: Env): Either[Err, Value] =
    Right(ValueNat)

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case Nat => true
      case _ => false
    }
  }

  /*
   -----------------
   ctx :- Nat => Universe
   */
  def infer(ctx: Ctx): Either[Err, The] =
    Right(The(Universe, Nat))
}
