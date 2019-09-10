package xieyuheng.tartlet

case class NeutralApply (
  fun: Neutral,
  arg: TheValue,
) extends Neutral {
  def readbackNeutral (ctx: Ctx): Either[ErrorMsg, Exp] = {
    for {
      rator <- fun.readbackNeutral(ctx)
      rand <- arg.readbackTheValue(ctx)
    } yield Apply(rator, rand)
  }
}
