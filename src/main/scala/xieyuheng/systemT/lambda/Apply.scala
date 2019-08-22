package xieyuheng.systemT

case class Apply (
  rator: Exp,
  rand: Exp,
) extends Eliminator {
  def eval(env: Env): Either[ErrorMsg, Value] = {
    for {
      fun <- rator.eval (env)
      arg <- rand.eval (env)
      value <- Apply.exe(fun, arg)
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
  def exe(fun: Value, arg: Value): Either[ErrorMsg, Value] = {
    fun match {
      case Closure(env, name, body) =>
        body.eval (env.ext (name, arg))
      case TheNeutral(theType, neutral) =>
        theType match {
          case Arrow(argType, retType) =>
            Right(TheNeutral(retType, NeutralApply(neutral, TheValue(argType, arg))))
          case _ =>
            Left(ErrorMsg(s"type of neutral fun is not Arrow: ${fun}"))
        }
      case _ =>
        Left(ErrorMsg(s"fun is not a closure: ${fun}"))
    }
  }
}
