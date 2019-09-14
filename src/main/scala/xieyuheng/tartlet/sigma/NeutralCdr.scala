package xieyuheng.tartlet

case class NeuCdr (
  pair: Neu,
) extends Neu {
  def readback_neu(ctx: Ctx): Either[Err, Exp] = {
    for {
      pair <- pair.readback_neu(ctx)
    } yield Cdr(pair)
  }
}
