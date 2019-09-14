package xieyuheng.tartlet

case class NeutralIndAbsurd (
  target: Neutral,
  motive: TheValue,
) extends Neutral {
  def readback_neu (ctx: Ctx): Either[Err, Exp] = {
    for {
      target <- target.readback_neu(ctx)
      motive <- motive.readback_the_val(ctx)
    } yield IndAbsurd(The(Absurd, target), motive)
  }
}
