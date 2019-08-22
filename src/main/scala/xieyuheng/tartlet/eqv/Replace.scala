package xieyuheng..tartlet

case class Replace (
  target: Exp,
  motive: Exp,
  base: Exp,
) extends Eliminator {
  def eval(env: Env): Either[ErrorMsg, Value] = {
    for {
      targetValue <- target.eval(env)
      motiveValue <- motive.eval(env)
      baseValue <- base.eval(env)
      res <- Replace.exe(
        targetValue,
        motiveValue,
        baseValue)
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
  def infer(ctx: Ctx): Either[ErrorMsg, The] = {
    for {
      the <- target.infer(ctx)
      res <- the.t match {
        case Eqv(t, from, to) =>
          for {
            typeValue <- t.eval(ctx.toEnv)
            motive <- motive.check(ctx, ValuePi(typeValue,
              NativeClosure("_", _ => Right(ValueUniverse))))
            motiveValue <- motive.eval(ctx.toEnv)
            fromValue <- from.eval(ctx.toEnv)
            baseType <- Apply.exe(motiveValue, fromValue)
            base <- base.check(ctx, baseType)
            toValue <- to.eval(ctx.toEnv)
            typeValue <- Apply.exe(motiveValue, toValue)
            typeExp <- typeValue.readBack(ctx, ValueUniverse)
          } yield The(typeExp, Replace(the.value, motive, base))
        case _ =>
          Left(ErrorMsg(
            s"expected the type to be Eqv(t, from, to), found: ${the.t}"))
      }
    } yield res
  }
}

object Replace {
  def exe(
    target: Value,
    motive: Value,
    base: Value,
  ): Either[ErrorMsg, Value] = {
    target match {
      case ValueSame =>
        Right(base)
      case TheNeutral(ValueEqv(t, from, to), neutral) => {
        for {
          typeValue <- Apply.exe(motive, to)
          baseType <- Apply.exe(motive, from)
        } yield TheNeutral(typeValue,
          NeutralReplace(
            neutral,
            TheValue(ValuePi(t, NativeClosure("x", _ => Right(ValueUniverse))), motive),
            TheValue(baseType, base)))
      }
      case _ =>
        Left(ErrorMsg(
          "target should be " +
            "ValueSame | " +
            "TheNeutral(ValueEqv(t, from, to), neutral): " +
            s"${target}"))
    }
  }
}
