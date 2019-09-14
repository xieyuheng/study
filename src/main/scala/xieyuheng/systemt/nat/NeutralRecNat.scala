package xieyuheng.systemt

case class NeuRecNat (
  t: Type,
  target: Neu,
  base: TheVal,
  step: TheVal,
) extends Neu {
  def readback_neu(usedNames: Set[String]): Either[Err, Exp] = {
    for {
      targetExp <- target.readback_neu(usedNames)
      baseExp <- base.readback_the_val(usedNames)
      stepExp <- step.readback_the_val(usedNames)
    } yield RecNat(t, targetExp, baseExp, stepExp)
  }
}
