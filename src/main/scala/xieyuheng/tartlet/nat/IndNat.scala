package xieyuheng.tartlet

case class NatInd (
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
      res <- NatInd.exe(
        targetVal,
        motiveVal,
        baseVal,
        stepVal)
    } yield res
  }

  def alpha_eq(
    that: Exp,
    this_map: Map[String, String],
    that_map: Map[String, String],
  ): Boolean = {
    that match {
      case NatInd(target2, motive2, base2, step2) =>
        target.alpha_eq(target2, this_map, that_map) &&
        motive.alpha_eq(motive2, this_map, that_map) &&
        base.alpha_eq(base2, this_map, that_map) &&
        step.alpha_eq(step2, this_map, that_map)
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
   ctx :- NatInd(target, motive, base, step) => motive(target)
   */
  def infer(ctx: Ctx): Either[Err, The] = {
    for {
      target <- target.check(ctx, ValNat)
      motive <- motive.check(ctx, ValPi(ValNat,
        NativeClo("n", _ => Right(ValUniverse))))
      motiveVal <- motive.eval(ctx.to_env)
      targetVal <- target.eval(ctx.to_env)
      baseType <- Ap.exe(motiveVal, ValZero)
      base <- base.check(ctx, baseType)
      step <- step.check(ctx, NatInd.stepType(motiveVal))
      typeVal <- Ap.exe(motiveVal, targetVal)
      t <- typeVal.readback_val(ctx, ValUniverse)
    } yield The(t, NatInd(target, motive, base, step))
   }
 }

object NatInd {
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
          almost <- NatInd.exe(prev, motive, base, step)
          res <- Ap.exe(f, almost)
        } yield res
      }
      case TheNeu(ValNat, neutral) => {
        for {
          t <- Ap.exe(motive, target)
          baseType <- Ap.exe(motive, ValZero)
        } yield TheNeu(t,
          NeuNatInd(
            neutral,
            TheVal(ValPi(ValNat, NativeClo("k", k => Right(ValUniverse))), motive),
            TheVal(baseType, base),
            TheVal(NatInd.stepType(motive), step)))
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
