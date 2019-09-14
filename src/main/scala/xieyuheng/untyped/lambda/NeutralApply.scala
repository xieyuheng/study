package xieyuheng.untyped

case class NeuApply (
  fn: Neu,
  arg: Val,
) extends Neu {
  def readback(used_names: Set[String]): Either[Err, Exp] = {
    for {
      rator <- fn.readback (used_names)
      rand <- arg.readback (used_names)
    } yield Apply(rator, rand)
  }
}
