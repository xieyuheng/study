package xieyuheng.tartlet

case object Universe extends Type {
  def eval(env: Env): Either[Err, Val] =
    Right(ValUniverse)

  def alpha_eq(
    that: Exp,
    this_map: Map[String, String],
    that_map: Map[String, String],
  ): Boolean = {
    that match {
      case Universe => true
      case _ => false
    }
  }

  /*
   -----------------
   ctx :- Universe => Universe
   */
  def infer(ctx: Ctx): Either[Err, The] =
    Right(The(Universe, Universe))
}
