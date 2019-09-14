package xieyuheng.tartlet

case class NeutralIndNat (
  target: Neutral,
  motive: TheValue,
  base: TheValue,
  step: TheValue,
) extends Neutral {
  def readback_neu(ctx: Ctx): Either[Err, Exp] = {
    for {
      target <- target.readback_neu(ctx)
      motive <- motive.readback_the_val(ctx)
      base <- base.readback_the_val(ctx)
      step <- step.readback_the_val(ctx)
    } yield IndNat(target, motive, base, step)
  }
}
