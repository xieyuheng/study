package xieyuheng.tartlet

case object Nat extends Type {
  def eval(env: Env): Either[Err, Val] =
    Right(ValNat)

  def alpha_eq(
    that: Exp,
    this_map: Map[String, String],
    that_map: Map[String, String],
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
