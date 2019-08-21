package xieyuheng.tt.tartlet

case class NeutralVar (
  name: String,
) extends Neutral {
  def readBackNeutral(ctx: Ctx): Either[ErrorMsg, Exp] = {
    Right(Var(name))
  }
}
