package xieyuheng.tartlet

case class TheNeutral (
  t: Value,
  neutral: Neutral,
) extends Value {
  def readBack (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] = {
    t match {
      case ValueAbsurd =>
        for {
          normal <- neutral.readBackNeutral(ctx)
        } yield The(Absurd, normal)
      case _ =>
        neutral.readBackNeutral(ctx)
    }
  }
}
