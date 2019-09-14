package xieyuheng.systemt

case class RecNat (
  t: Type,
  target: Exp,
  base: Exp,
  step: Exp,
) extends Eliminator {
  def eval(env: Env): Either[Err, Val] = {
    for {
      targetVal <- target.eval (env)
      baseVal <- base.eval (env)
      stepVal <- step.eval (env)
      res <- RecNat.exe(
        t,
        targetVal,
        baseVal,
        stepVal)
    } yield res
  }

  /*
   ctx :- target <= Nat
   ctx :- base <= T
   ctx :- step <= Nat -> T -> T
   -----------------------------------
   ctx :- RecNat [T] (target, base, step) => T
   */
  def infer(ctx: Ctx): Either[Err, Type] = {
    for {
      _ok <- target.check(ctx, Nat)
      _ok <- base.check(ctx, t)
      _ok <- step.check(ctx, Arrow(Nat, Arrow(t, t)))
    } yield t
  }
}

case object RecNat {
  def exe(
    t: Type,
    target: Val,
    base: Val,
    step: Val,
  ): Either[Err, Val] = {
    target match {
      case ValZero =>
        Right(base)
      case ValAdd1(prev) =>
        for {
          f <- Ap.exe(step, prev)
          almost <- RecNat.exe(t, prev, base, step)
          res <- Ap.exe(f, almost)
        } yield res
      case TheNeu(theType, neutral) =>
        theType match {
          case Nat =>
            Right(TheNeu(t, NeuRecNat(
              t,
              neutral,
              TheVal(t, base),
              TheVal(Arrow(Nat, Arrow(t, t)), step))))
          case _ =>
            Left(Err(s"type of the neutral target is not Nat: ${theType}"))
        }
      case _ =>
        Left(Err(
          s"target of RecNat is not ValZero or ValAdd1: ${target}"))
    }
  }
}
