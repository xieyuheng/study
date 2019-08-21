package xieyuheng.tt.tartlet

case class NeutralIndNat (
  target: Neutral,
  motive: TheValue,
  base: TheValue,
  step: TheValue,
) extends Neutral {
  def readBackNeutral(ctx: Ctx): Either[ErrorMsg, Exp] = {
    for {
      target <- target.readBackNeutral(ctx)
      motive <- motive.readBackTheValue(ctx)
      base <- base.readBackTheValue(ctx)
      step <- step.readBackTheValue(ctx)
    } yield IndNat(target, motive, base, step)
  }
}
