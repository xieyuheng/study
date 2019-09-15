package xieyuheng.tartlet

case class Ap (
  rator: Exp,
  rand: Exp,
) extends Eliminator {
  def eval(env: Env): Either[Err, Val] = {
    for {
      fn <- rator.eval(env)
      arg <- rand.eval(env)
      res <- Ap.exe(fn, arg)
    } yield res
  }

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case Ap(rator2, rand2) =>
        rator.alphaEq(rator2, thisMap, thatMap) &&
        rand.alphaEq(rand2, thisMap, thatMap)
      case _ => false
    }
  }

  /*
   ctx :- rator => Pi(x: A, R)
   ctx :- rand <= A
   -----------------
   ctx :- Ap(rator, rand) => R
   */
  def infer(ctx: Ctx): Either[Err, The] = {
    for {
      the <- rator.infer(ctx)
      t <- the.t.eval(ctx.toEnv)
      res <- t match {
        case ValPi(argType, retType) => {
          for {
            rand <- rand.check(ctx, argType)
            argVal <- rand.eval(ctx.toEnv)
            retVal <- retType.apply(argVal)
            retExp <- retVal.readback_val(ctx, ValUniverse)
          } yield The(retExp, Ap(the.value, rand))
        }
        case _ =>
          Left(Err("expected Pi, " + "found: ${t}"))
      }
    } yield res
  }
}

object Ap {
  def exe(
    fn: Val,
    arg: Val,
  ): Either[Err, Val] = {
    fn match {
      case ValLambda(clo) =>
        clo.apply(arg)
      case TheNeu(ValPi(argType, retType), neutral) =>
        for {
          t <- retType.apply(arg)
        } yield TheNeu(t, NeuAp(neutral, TheVal(argType, arg)))
      case _ =>
        Left(Err(
          "fn should be " +
            "ValLambda(clo) | " +
            "TheNeu(ValPi(argType, retType), neutral): " +
            s"${fn}"))
    }
  }
}
