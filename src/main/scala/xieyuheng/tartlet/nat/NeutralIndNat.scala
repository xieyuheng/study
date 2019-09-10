package xieyuheng.tartlet

case class NeutralIndNat (
  target: Neutral,
  motive: TheValue,
  base: TheValue,
  step: TheValue,
) extends Neutral {
  def readbackNeutral(ctx: Ctx): Either[ErrorMsg, Exp] = {
    for {
      target <- target.readbackNeutral(ctx)
      motive <- motive.readbackTheValue(ctx)
      base <- base.readbackTheValue(ctx)
      step <- step.readbackTheValue(ctx)
    } yield IndNat(target, motive, base, step)
  }
}
