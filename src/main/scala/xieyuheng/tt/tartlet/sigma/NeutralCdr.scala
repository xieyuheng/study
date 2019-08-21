package xieyuheng.tt.tartlet

case class NeutralCdr (
  pair: Neutral,
) extends Neutral {
  def readBackNeutral(ctx: Ctx): Either[ErrorMsg, Exp] = {
    for {
      pair <- pair.readBackNeutral(ctx)
    } yield Cdr(pair)
  }
}
