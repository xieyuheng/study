package xieyuheng.tt.tartlet

case class Apply (
  rator: Exp,
  rand: Exp,
) extends Eliminator {
  def eval(env: Env): Either[ErrorMsg, Value] = {
    for {
      fun <- rator.eval(env)
      arg <- rand.eval(env)
      res <- Apply.exe(fun, arg)
    } yield res
  }

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case Apply(rator2, rand2) =>
        rator.alphaEq(rator2, thisMap, thatMap) &&
        rand.alphaEq(rand2, thisMap, thatMap)
      case _ => false
    }
  }

  /*
   ctx :- rator => Pi(x: A, R)
   ctx :- rand <= A
   -----------------
   ctx :- Apply(rator, rand) => R
   */
  def infer(ctx: Ctx): Either[ErrorMsg, The] = {
    for {
      the <- rator.infer(ctx)
      t <- the.t.eval(ctx.toEnv)
      res <- t match {
        case ValuePi(argType, retType) => {
          for {
            rand <- rand.check(ctx, argType)
            argValue <- rand.eval(ctx.toEnv)
            retValue <- retType.apply(argValue)
            retExp <- retValue.readBack(ctx, ValueUniverse)
          } yield The(retExp, Apply(the.value, rand))
        }
        case _ =>
          Left(ErrorMsg("expected Pi, " + "found: ${t}"))
      }
    } yield res
  }
}

object Apply {
  def exe(
    fun: Value,
    arg: Value,
  ): Either[ErrorMsg, Value] = {
    fun match {
      case ValueLambda(closure) =>
        closure.apply(arg)
      case TheNeutral(ValuePi(argType, retType), neutral) =>
        for {
          t <- retType.apply(arg)
        } yield TheNeutral(t, NeutralApply(neutral, TheValue(argType, arg)))
      case _ =>
        Left(ErrorMsg(
          "fun should be " +
            "ValueLambda(closure) | " +
            "TheNeutral(ValuePi(argType, retType), neutral): " +
            s"${fun}"))
    }
  }
}
