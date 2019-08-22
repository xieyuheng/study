package xieyuheng.tartlet

case class NeutralCar (
  pair: Neutral,
) extends Neutral {
  def readBackNeutral(ctx: Ctx): Either[ErrorMsg, Exp] = {
    for {
      pair <- pair.readBackNeutral(ctx)
    } yield Car(pair)
  }
}
