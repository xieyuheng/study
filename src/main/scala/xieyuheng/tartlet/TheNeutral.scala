package xieyuheng.tartlet

case class TheNeutral (
  t: Value,
  neutral: Neutral,
) extends Value {
  def readback (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] = {
    t match {
      case ValueAbsurd =>
        for {
          normal <- neutral.readbackNeutral(ctx)
        } yield The(Absurd, normal)
      case _ =>
        neutral.readbackNeutral(ctx)
    }
  }
}
