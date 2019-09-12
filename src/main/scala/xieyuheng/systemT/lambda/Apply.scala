package xieyuheng.systemT

case class Apply (
  rator: Exp,
  rand: Exp,
) extends Eliminator {
  def eval(env: Env): Either[ErrorMsg, Value] = {
    for {
      fn <- rator.eval (env)
      arg <- rand.eval (env)
      value <- Apply.exe(fn, arg)
    } yield value
  }

  /*
   ctx :- rator => A -> R
   ctx :- rand <= A
   ---------------
   ctx :- Apply (rator, rand) => R
   */
  def infer(ctx: Ctx): Either[ErrorMsg, Type] = {
    rator.infer(ctx) match {
      case Right(Arrow(argType, retType)) =>
        for {
          _ok <- rand.check(ctx, argType)
        } yield retType
      case Left(errorMsg) =>
        Left(errorMsg)
      case _ =>
        Left(ErrorMsg(s"the type of rator: ${rator} is not Arrow"))
    }
  }
}

case object Apply {
  def exe(fn: Value, arg: Value): Either[ErrorMsg, Value] = {
    fn match {
      case Closure(env, name, body) =>
        body.eval (env.ext (name, arg))
      case TheNeutral(theType, neutral) =>
        theType match {
          case Arrow(argType, retType) =>
            Right(TheNeutral(retType, NeutralApply(neutral, TheValue(argType, arg))))
          case _ =>
            Left(ErrorMsg(s"type of neutral fn is not Arrow: ${fn}"))
        }
      case _ =>
        Left(ErrorMsg(s"fn is not a closure: ${fn}"))
    }
  }
}
