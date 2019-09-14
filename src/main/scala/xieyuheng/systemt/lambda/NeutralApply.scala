package xieyuheng.systemt

case class NeutralApply (
  fn: Neutral,
  arg: TheValue,
) extends Neutral {
  def readback_neu(usedNames: Set[String]): Either[Err, Exp] = {
    for {
      rator <- fn.readback_neu (usedNames)
      rand <- arg.readback_the_val (usedNames)
    } yield Apply(rator, rand)
  }
}
