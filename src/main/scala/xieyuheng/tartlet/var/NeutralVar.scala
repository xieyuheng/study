package xieyuheng.tartlet

case class NeutralVar (
  name: String,
) extends Neutral {
  def readback_neu(ctx: Ctx): Either[Err, Exp] = {
    Right(Var(name))
  }
}
