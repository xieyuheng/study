package xieyuheng.untyped

case class NeuApply (
  fn: Neu,
  arg: Val,
) extends Neu {
  def readback(usedNames: Set[String]): Either[Err, Exp] = {
    for {
      rator <- fn.readback (usedNames)
      rand <- arg.readback (usedNames)
    } yield Apply(rator, rand)
  }
}
