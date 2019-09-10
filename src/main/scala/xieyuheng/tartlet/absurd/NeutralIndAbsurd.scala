package xieyuheng.tartlet

case class NeutralIndAbsurd (
  target: Neutral,
  motive: TheValue,
) extends Neutral {
  def readbackNeutral (ctx: Ctx): Either[ErrorMsg, Exp] = {
    for {
      target <- target.readbackNeutral(ctx)
      motive <- motive.readbackTheValue(ctx)
    } yield IndAbsurd(The(Absurd, target), motive)
  }
}
