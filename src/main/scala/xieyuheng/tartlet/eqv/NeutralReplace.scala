package xieyuheng..tartlet

case class NeutralReplace (
  target: Neutral,
  motive: TheValue,
  base: TheValue,
) extends Neutral {
  def readBackNeutral(ctx: Ctx): Either[ErrorMsg, Exp] = {
    for {
      target <- target.readBackNeutral(ctx)
      motive <- motive.readBackTheValue(ctx)
      base <- base.readBackTheValue(ctx)
    } yield Replace(target, motive, base)
  }
}
