package xieyuheng..tartlet

case object Absurd extends Type {
  def eval(env: Env): Either[ErrorMsg, Value] =
    Right(ValueAbsurd)

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case Absurd => true
      case _ => false
    }
  }

  /*
   -----------------
   ctx :- Absurd => Universe
   */
  def infer(ctx: Ctx): Either[ErrorMsg, The] =
    Right(The(Universe, Absurd))
}
