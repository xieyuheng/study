package xieyuheng.systemT

case class NeutralApply (
  fun: Neutral,
  arg: TheValue,
) extends Neutral {
  def readbackNeutral(usedNames: Set[String]): Either[ErrorMsg, Exp] = {
    for {
      rator <- fun.readbackNeutral (usedNames)
      rand <- arg.readbackTheValue (usedNames)
    } yield Apply(rator, rand)
  }
}
