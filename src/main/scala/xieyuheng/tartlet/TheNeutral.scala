package xieyuheng.tartlet

case class TheNeutral (
  t: Value,
  neutral: Neutral,
) extends Value {
  def readback (ctx: Ctx, t: Value): Either[Err, Exp] = {
    t match {
      case ValueAbsurd =>
        for {
          normal <- neutral.readback_neu(ctx)
        } yield The(Absurd, normal)
      case _ =>
        neutral.readback_neu(ctx)
    }
  }
}
