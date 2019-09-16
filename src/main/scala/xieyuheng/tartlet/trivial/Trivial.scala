package xieyuheng.tartlet

case object Trivial extends Type {
  def eval(env: Env): Either[Err, Val] =
    Right(ValTrivial)

  def alpha_eq(
    that: Exp,
    this_map: Map[String, String],
    that_map: Map[String, String],
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
  def infer(ctx: Ctx): Either[Err, The] =
    Right(The(Universe, Trivial))
}
