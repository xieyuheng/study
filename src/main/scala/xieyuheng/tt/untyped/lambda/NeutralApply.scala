package xieyuheng.tt.untyped

case class NeutralApply (
  fun: Neutral,
  arg: Value,
) extends Neutral {
  def readBack(usedNames: Set[String]): Either[ErrorMsg, Exp] = {
    for {
      rator <- fun.readBack (usedNames)
      rand <- arg.readBack (usedNames)
    } yield Apply(rator, rand)
  }
}
