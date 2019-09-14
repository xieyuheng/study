package xieyuheng.tartlet

case class IndNat (
  target: Exp,
  motive: Exp,
  base: Exp,
  step: Exp,
) extends Eliminator {
  def eval(env: Env): Either[Err, Value] = {
    for {
      targetValue <- target.eval(env)
      motiveValue <- motive.eval(env)
      baseValue <- base.eval(env)
      stepValue <- step.eval(env)
      res <- IndNat.exe(
        targetValue,
        motiveValue,
        baseValue,
        stepValue)
    } yield res
  }

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case IndNat(target2, motive2, base2, step2) =>
        target.alphaEq(target2, thisMap, thatMap) &&
        motive.alphaEq(motive2, thisMap, thatMap) &&
        base.alphaEq(base2, thisMap, thatMap) &&
        step.alphaEq(step2, thisMap, thatMap)
      case _ => false
    }
  }

  /*
   ctx :- target <= Nat
   ctx :- motive <= Pi(_: Nat, Universe)
   ctx :- base <= motive(Zero)
   ctx :- step <= Pi(
   --          prev: Nat, Pi(
   --            almost: motive(prev), motive(Add1(prev))))
   --------------------
   ctx :- IndNat(target, motive, base, step) => motive(target)
   */
  def infer(ctx: Ctx): Either[Err, The] = {
    for {
      target <- target.check(ctx, ValueNat)
      motive <- motive.check(ctx, ValuePi(ValueNat,
        NativeClosure("n", _ => Right(ValueUniverse))))
      motiveValue <- motive.eval(ctx.toEnv)
      targetValue <- target.eval(ctx.toEnv)
      baseType <- Apply.exe(motiveValue, ValueZero)
      base <- base.check(ctx, baseType)
      step <- step.check(ctx, IndNat.stepType(motiveValue))
      typeValue <- Apply.exe(motiveValue, targetValue)
      t <- typeValue.readback(ctx, ValueUniverse)
    } yield The(t, IndNat(target, motive, base, step))
   }
 }

object IndNat {
  def stepType(motive: Value): ValuePi = {
    ValuePi(ValueNat,
      NativeClosure("prev", prev =>
        for {
          almostType <- Apply.exe(motive, prev)
        } yield ValuePi(almostType,
          NativeClosure("almost", almost =>
            Apply.exe(motive, ValueAdd1(prev))))))
  }

  def exe(
    target: Value,
    motive: Value,
    base: Value,
    step: Value,
  ): Either[Err, Value] = {
    target match {
      case ValueZero =>
        Right(base)
      case ValueAdd1(prev) => {
        for {
          f <- Apply.exe(step, prev)
          almost <- IndNat.exe(prev, motive, base, step)
          res <- Apply.exe(f, almost)
        } yield res
      }
      case TheNeutral(ValueNat, neutral) => {
        for {
          t <- Apply.exe(motive, target)
          baseType <- Apply.exe(motive, ValueZero)
        } yield TheNeutral(t,
          NeutralIndNat(
            neutral,
            TheValue(ValuePi(ValueNat, NativeClosure("k", k => Right(ValueUniverse))), motive),
            TheValue(baseType, base),
            TheValue(IndNat.stepType(motive), step)))
      }
      case _ =>
        Left(Err(
          "target should be " +
            "ValueZero | " +
            "ValueAdd1(prev) | " +
            "TheNeutral(ValueNat, neutral): " +
            s"${target}"))
    }
  }
}
