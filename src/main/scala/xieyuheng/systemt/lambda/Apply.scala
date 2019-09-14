package xieyuheng.systemt

case class Ap (
  rator: Exp,
  rand: Exp,
) extends Eliminator {
  def eval(env: Env): Either[Err, Val] = {
    for {
      fn <- rator.eval (env)
      arg <- rand.eval (env)
      value <- Ap.exe(fn, arg)
    } yield value
  }

  /*
   ctx :- rator => A -> R
   ctx :- rand <= A
   ---------------
   ctx :- Ap (rator, rand) => R
   */
  def infer(ctx: Ctx): Either[Err, Type] = {
    rator.infer(ctx) match {
      case Right(Arrow(argType, retType)) =>
        for {
          _ok <- rand.check(ctx, argType)
        } yield retType
      case Left(errorMsg) =>
        Left(errorMsg)
      case _ =>
        Left(Err(s"the type of rator: ${rator} is not Arrow"))
    }
  }
}

case object Ap {
  def exe(fn: Val, arg: Val): Either[Err, Val] = {
    fn match {
      case Closure(env, name, body) =>
        body.eval (env.ext (name, arg))
      case TheNeu(theType, neutral) =>
        theType match {
          case Arrow(argType, retType) =>
            Right(TheNeu(retType, NeuAp(neutral, TheVal(argType, arg))))
          case _ =>
            Left(Err(s"type of neutral fn is not Arrow: ${fn}"))
        }
      case _ =>
        Left(Err(s"fn is not a closure: ${fn}"))
    }
  }
}
