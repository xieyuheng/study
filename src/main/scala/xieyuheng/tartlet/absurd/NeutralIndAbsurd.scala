package xieyuheng.tartlet

case class NeutralIndAbsurd (
  target: Neutral,
  motive: TheValue,
) extends Neutral {
  def readBackNeutral (ctx: Ctx): Either[ErrorMsg, Exp] = {
    for {
      target <- target.readBackNeutral(ctx)
      motive <- motive.readBackTheValue(ctx)
    } yield IndAbsurd(The(Absurd, target), motive)
  }
}
