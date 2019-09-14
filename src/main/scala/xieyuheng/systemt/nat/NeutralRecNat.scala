package xieyuheng.systemt

case class NeutralRecNat (
  t: Type,
  target: Neutral,
  base: TheValue,
  step: TheValue,
) extends Neutral {
  def readback_neu(usedNames: Set[String]): Either[Err, Exp] = {
    for {
      targetExp <- target.readback_neu(usedNames)
      baseExp <- base.readback_the_val(usedNames)
      stepExp <- step.readback_the_val(usedNames)
    } yield RecNat(t, targetExp, baseExp, stepExp)
  }
}
