package xieyuheng.tartlet

case class NeuNatInd (
  target: Neu,
  motive: TheVal,
  base: TheVal,
  step: TheVal,
) extends Neu {
  def readback_neu(ctx: Ctx): Either[Err, Exp] = {
    for {
      target <- target.readback_neu(ctx)
      motive <- motive.readback_the_val(ctx)
      base <- base.readback_the_val(ctx)
      step <- step.readback_the_val(ctx)
    } yield NatInd(target, motive, base, step)
  }
}
