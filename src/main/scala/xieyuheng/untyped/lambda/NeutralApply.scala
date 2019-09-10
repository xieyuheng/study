package xieyuheng.untyped

case class NeutralApply (
  fun: Neutral,
  arg: Value,
) extends Neutral {
  def readback(usedNames: Set[String]): Either[ErrorMsg, Exp] = {
    for {
      rator <- fun.readback (usedNames)
      rand <- arg.readback (usedNames)
    } yield Apply(rator, rand)
  }
}
