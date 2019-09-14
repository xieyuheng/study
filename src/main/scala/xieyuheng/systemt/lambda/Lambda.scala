package xieyuheng.systemt

case class Lambda (
  name: String,
  body: Exp,
) extends Constructor {
  def eval(env: Env): Either[Err, Value] = {
    Right(Closure(env, name, body))
  }

  /*
   ctx.ext (x, A) :- e <= B
   -------------------------
   ctx :- Lambda (x, e) <= A -> B
   */
  def check(ctx: Ctx, t: Type): Either[Err, Unit] = {
    t match {
      case Arrow(argType, retType) =>
        body.check(ctx.ext(name, argType), retType)
      case _ =>
        Left(Err(
          s"type of Lambda is not Arrow, exp: ${this}"))
    }
  }
}
