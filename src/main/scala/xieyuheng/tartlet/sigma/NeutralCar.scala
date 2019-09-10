package xieyuheng.tartlet

case class NeutralCar (
  pair: Neutral,
) extends Neutral {
  def readbackNeutral(ctx: Ctx): Either[ErrorMsg, Exp] = {
    for {
      pair <- pair.readbackNeutral(ctx)
    } yield Car(pair)
  }
}
