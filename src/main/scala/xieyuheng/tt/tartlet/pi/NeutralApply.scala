package xieyuheng.tt.tartlet

case class NeutralApply (
  fun: Neutral,
  arg: TheValue,
) extends Neutral {
  def readBackNeutral (ctx: Ctx): Either[ErrorMsg, Exp] = {
    for {
      rator <- fun.readBackNeutral(ctx)
      rand <- arg.readBackTheValue(ctx)
    } yield Apply(rator, rand)
  }
}
