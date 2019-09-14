package xieyuheng.tartlet

case class NeutralCar (
  pair: Neutral,
) extends Neutral {
  def readback_neu(ctx: Ctx): Either[Err, Exp] = {
    for {
      pair <- pair.readback_neu(ctx)
    } yield Car(pair)
  }
}
