package xieyuheng.tartlet

case class NeutralCdr (
  pair: Neutral,
) extends Neutral {
  def readbackNeutral(ctx: Ctx): Either[ErrorMsg, Exp] = {
    for {
      pair <- pair.readbackNeutral(ctx)
    } yield Cdr(pair)
  }
}
