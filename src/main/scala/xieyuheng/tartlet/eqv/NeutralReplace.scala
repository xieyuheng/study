package xieyuheng.tartlet

case class NeutralReplace (
  target: Neutral,
  motive: TheValue,
  base: TheValue,
) extends Neutral {
  def readbackNeutral(ctx: Ctx): Either[ErrorMsg, Exp] = {
    for {
      target <- target.readbackNeutral(ctx)
      motive <- motive.readbackTheValue(ctx)
      base <- base.readbackTheValue(ctx)
    } yield Replace(target, motive, base)
  }
}
