package xieyuheng.untyped

case class NeutralApply (
  fn: Neutral,
  arg: Value,
) extends Neutral {
  def readback(usedNames: Set[String]): Either[Err, Exp] = {
    for {
      rator <- fn.readback (usedNames)
      rand <- arg.readback (usedNames)
    } yield Apply(rator, rand)
  }
}
