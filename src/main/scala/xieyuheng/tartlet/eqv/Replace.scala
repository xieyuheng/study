package xieyuheng.tartlet

case class Replace (
  target: Exp,
  motive: Exp,
  base: Exp,
) extends Eliminator {
  def eval(env: Env): Either[Err, Val] = {
    for {
      targetVal <- target.eval(env)
      motiveVal <- motive.eval(env)
      baseVal <- base.eval(env)
      res <- Replace.exe(
        targetVal,
        motiveVal,
        baseVal)
    } yield res
  }

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case Replace(target2, motive2, base2) =>
        target.alphaEq(target2, thisMap, thatMap) &&
        motive.alphaEq(motive2, thisMap, thatMap) &&
        base.alphaEq(base2, thisMap, thatMap)
      case _ => false
    }
  }

  /*
   ctx :- target => Eqv(T, from, to)
   ctx :- motive <= Pi(_: T, Universe)
   ctx :- base <= motive(from)
   --------------------
   ctx :- Replace(target, motive, base) => motive(to)
   */
  def infer(ctx: Ctx): Either[Err, The] = {
    for {
      the <- target.infer(ctx)
      res <- the.t match {
        case Eqv(t, from, to) =>
          for {
            typeVal <- t.eval(ctx.toEnv)
            motive <- motive.check(ctx, ValPi(typeVal,
              NativeClo("_", _ => Right(ValUniverse))))
            motiveVal <- motive.eval(ctx.toEnv)
            fromVal <- from.eval(ctx.toEnv)
            baseType <- Ap.exe(motiveVal, fromVal)
            base <- base.check(ctx, baseType)
            toVal <- to.eval(ctx.toEnv)
            typeVal <- Ap.exe(motiveVal, toVal)
            typeExp <- typeVal.readback_val(ctx, ValUniverse)
          } yield The(typeExp, Replace(the.value, motive, base))
        case _ =>
          Left(Err(
            s"expected the type to be Eqv(t, from, to), found: ${the.t}"))
      }
    } yield res
  }
}

object Replace {
  def exe(
    target: Val,
    motive: Val,
    base: Val,
  ): Either[Err, Val] = {
    target match {
      case ValSame =>
        Right(base)
      case TheNeu(ValEqv(t, from, to), neutral) => {
        for {
          typeVal <- Ap.exe(motive, to)
          baseType <- Ap.exe(motive, from)
        } yield TheNeu(typeVal,
          NeuReplace(
            neutral,
            TheVal(ValPi(t, NativeClo("x", _ => Right(ValUniverse))), motive),
            TheVal(baseType, base)))
      }
      case _ =>
        Left(Err(
          "target should be " +
            "ValSame | " +
            "TheNeu(ValEqv(t, from, to), neutral): " +
            s"${target}"))
    }
  }
}
