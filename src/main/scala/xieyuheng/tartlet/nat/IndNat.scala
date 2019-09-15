package xieyuheng.tartlet

case class IndNat (
  target: Exp,
  motive: Exp,
  base: Exp,
  step: Exp,
) extends Eliminator {
  def eval(env: Env): Either[Err, Val] = {
    for {
      targetVal <- target.eval(env)
      motiveVal <- motive.eval(env)
      baseVal <- base.eval(env)
      stepVal <- step.eval(env)
      res <- IndNat.exe(
        targetVal,
        motiveVal,
        baseVal,
        stepVal)
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
      target <- target.check(ctx, ValNat)
      motive <- motive.check(ctx, ValPi(ValNat,
        NativeClo("n", _ => Right(ValUniverse))))
      motiveVal <- motive.eval(ctx.toEnv)
      targetVal <- target.eval(ctx.toEnv)
      baseType <- Ap.exe(motiveVal, ValZero)
      base <- base.check(ctx, baseType)
      step <- step.check(ctx, IndNat.stepType(motiveVal))
      typeVal <- Ap.exe(motiveVal, targetVal)
      t <- typeVal.readback_val(ctx, ValUniverse)
    } yield The(t, IndNat(target, motive, base, step))
   }
 }

object IndNat {
  def stepType(motive: Val): ValPi = {
    ValPi(ValNat,
      NativeClo("prev", prev =>
        for {
          almostType <- Ap.exe(motive, prev)
        } yield ValPi(almostType,
          NativeClo("almost", almost =>
            Ap.exe(motive, ValAdd1(prev))))))
  }

  def exe(
    target: Val,
    motive: Val,
    base: Val,
    step: Val,
  ): Either[Err, Val] = {
    target match {
      case ValZero =>
        Right(base)
      case ValAdd1(prev) => {
        for {
          f <- Ap.exe(step, prev)
          almost <- IndNat.exe(prev, motive, base, step)
          res <- Ap.exe(f, almost)
        } yield res
      }
      case TheNeu(ValNat, neutral) => {
        for {
          t <- Ap.exe(motive, target)
          baseType <- Ap.exe(motive, ValZero)
        } yield TheNeu(t,
          NeuIndNat(
            neutral,
            TheVal(ValPi(ValNat, NativeClo("k", k => Right(ValUniverse))), motive),
            TheVal(baseType, base),
            TheVal(IndNat.stepType(motive), step)))
      }
      case _ =>
        Left(Err(
          "target should be " +
            "ValZero | " +
            "ValAdd1(prev) | " +
            "TheNeu(ValNat, neutral): " +
            s"${target}"))
    }
  }
}
