package xieyuheng.tartlet

case class NeuApply (
  fn: Neu,
  arg: TheVal,
) extends Neu {
  def readback_neu (ctx: Ctx): Either[Err, Exp] = {
    for {
      rator <- fn.readback_neu(ctx)
      rand <- arg.readback_the_val(ctx)
    } yield Apply(rator, rand)
  }
}
