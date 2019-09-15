package xieyuheng.tartlet

case class TheNeu (
  t: Val,
  neutral: Neu,
) extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] = {
    t match {
      case ValAbsurd =>
        for {
          normal <- neutral.readback_neu(ctx)
        } yield The(Absurd, normal)
      case _ =>
        neutral.readback_neu(ctx)
    }
  }
}
