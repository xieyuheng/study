package xieyuheng.tartlet

case object Absurd extends Type {
  def eval(env: Env): Either[Err, Val] =
    Right(ValAbsurd)

  def alpha_eq(
    that: Exp,
    this_map: Map[String, String],
    that_map: Map[String, String],
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
  def infer(ctx: Ctx): Either[Err, The] =
    Right(The(Universe, Absurd))
}
