package xieyuheng.tartlet

case class Var (name: String) extends Eliminator {
  def eval(env: Env): Either[Err, Val] = {
    env.lookup_val(name) match {
      case Some(value) =>
        Right(value)
      case None =>
        Left(Err(s"can not find var: ${this} in env: ${env}"))
    }
  }

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case Var(name2) =>
        (thisMap.get(name), thisMap.get(name2)) match {
          case (Some(sym), Some(sym2)) =>
            sym == sym2
          case (None, None) =>
            name == name2
          case _ =>
            false
        }
      case _ =>
        false
    }
  }

  /*
   ctx.lookup_type(x) == T
   --------------------------
   ctx :- Var(x) => T
   */
  def infer(ctx: Ctx): Either[Err, The] = {
    ctx.lookup_type(name) match {
      case Some(typeVal) => {
        for {
          typeExp <- typeVal.readback_val(ctx, ValUniverse)
        } yield The(typeExp, this)
      }
      case None =>
        Left(Err(s"can not find var: ${this} in ctx"))
    }
  }
}
