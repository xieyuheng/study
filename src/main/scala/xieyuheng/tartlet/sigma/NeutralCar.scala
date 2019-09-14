package xieyuheng.tartlet

case class NeuCar (
  pair: Neu,
) extends Neu {
  def readback_neu(ctx: Ctx): Either[Err, Exp] = {
    for {
      pair <- pair.readback_neu(ctx)
    } yield Car(pair)
  }
}
