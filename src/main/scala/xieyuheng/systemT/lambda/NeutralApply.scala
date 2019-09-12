package xieyuheng.systemT

case class NeutralApply (
  fn: Neutral,
  arg: TheValue,
) extends Neutral {
  def readbackNeutral(usedNames: Set[String]): Either[ErrorMsg, Exp] = {
    for {
      rator <- fn.readbackNeutral (usedNames)
      rand <- arg.readbackTheValue (usedNames)
    } yield Apply(rator, rand)
  }
}
