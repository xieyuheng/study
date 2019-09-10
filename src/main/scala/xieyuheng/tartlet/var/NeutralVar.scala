package xieyuheng.tartlet

case class NeutralVar (
  name: String,
) extends Neutral {
  def readbackNeutral(ctx: Ctx): Either[ErrorMsg, Exp] = {
    Right(Var(name))
  }
}
