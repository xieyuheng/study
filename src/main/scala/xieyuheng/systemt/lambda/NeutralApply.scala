package xieyuheng.systemt

case class NeuApply (
  fn: Neu,
  arg: TheVal,
) extends Neu {
  def readback_neu(usedNames: Set[String]): Either[Err, Exp] = {
    for {
      rator <- fn.readback_neu (usedNames)
      rand <- arg.readback_the_val (usedNames)
    } yield Apply(rator, rand)
  }
}
