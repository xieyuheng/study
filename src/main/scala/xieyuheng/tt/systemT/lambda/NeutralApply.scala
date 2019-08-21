package xieyuheng.tt.systemT

case class NeutralApply (
  fun: Neutral,
  arg: TheValue,
) extends Neutral {
  def readBackNeutral(usedNames: Set[String]): Either[ErrorMsg, Exp] = {
    for {
      rator <- fun.readBackNeutral (usedNames)
      rand <- arg.readBackTheValue (usedNames)
    } yield Apply(rator, rand)
  }
}
