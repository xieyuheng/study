package xieyuheng.tartlet

case class NeuReplace (
  target: Neu,
  motive: TheVal,
  base: TheVal,
) extends Neu {
  def readback_neu(ctx: Ctx): Either[Err, Exp] = {
    for {
      target <- target.readback_neu(ctx)
      motive <- motive.readback_the_val(ctx)
      base <- base.readback_the_val(ctx)
    } yield Replace(target, motive, base)
  }
}
