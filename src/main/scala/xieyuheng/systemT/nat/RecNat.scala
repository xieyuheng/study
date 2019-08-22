package xieyuheng.systemT

case class RecNat (
  t: Type,
  target: Exp,
  base: Exp,
  step: Exp,
) extends Eliminator {
  def eval(env: Env): Either[ErrorMsg, Value] = {
    for {
      targetValue <- target.eval (env)
      baseValue <- base.eval (env)
      stepValue <- step.eval (env)
      res <- RecNat.exe(
        t,
        targetValue,
        baseValue,
        stepValue)
    } yield res
  }

  /*
   ctx :- target <= Nat
   ctx :- base <= T
   ctx :- step <= Nat -> T -> T
   -----------------------------------
   ctx :- RecNat [T] (target, base, step) => T
   */
  def infer(ctx: Ctx): Either[ErrorMsg, Type] = {
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
    target: Value,
    base: Value,
    step: Value,
  ): Either[ErrorMsg, Value] = {
    target match {
      case ValueZero =>
        Right(base)
      case ValueAdd1(prev) =>
        for {
          f <- Apply.exe(step, prev)
          almost <- RecNat.exe(t, prev, base, step)
          res <- Apply.exe(f, almost)
        } yield res
      case TheNeutral(theType, neutral) =>
        theType match {
          case Nat =>
            Right(TheNeutral(t, NeutralRecNat(
              t,
              neutral,
              TheValue(t, base),
              TheValue(Arrow(Nat, Arrow(t, t)), step))))
          case _ =>
            Left(ErrorMsg(s"type of the neutral target is not Nat: ${theType}"))
        }
      case _ =>
        Left(ErrorMsg(
          s"target of RecNat is not ValueZero or ValueAdd1: ${target}"))
    }
  }
}
