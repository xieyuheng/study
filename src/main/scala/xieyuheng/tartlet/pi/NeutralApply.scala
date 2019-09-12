package xieyuheng.tartlet

case class NeutralApply (
  fn: Neutral,
  arg: TheValue,
) extends Neutral {
  def readbackNeutral (ctx: Ctx): Either[ErrorMsg, Exp] = {
    for {
      rator <- fn.readbackNeutral(ctx)
      rand <- arg.readbackTheValue(ctx)
    } yield Apply(rator, rand)
  }
}
