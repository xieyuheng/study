package xieyuheng.tartlet

import java.util.UUID

case class Lambda (
  name: String,
  body: Exp,
) extends Constructor {
  def eval(env: Env): Either[Err, Val] =
    Right(ValLambda(EnvClo(env, name, body)))

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case Lambda(name2, body2) => {
        val sym = UUID.randomUUID().toString
        body.alphaEq(body2, thisMap + (name -> sym), thatMap + (name2 -> sym))
      }
      case _ => false
    }
  }

  /*
    ctx.ext(x, A) :- body <= R
    ------------------------------
    ctx :- Lambda(x, body) <= Pi
   */
  def check(ctx: Ctx, t: Val): Either[Err, Exp] =
    t match {
      case ValPi(arg_t, ret_t) => {
        val varVal = TheNeu(arg_t, NeuVar(name))
        for {
          realRetType <- ret_t.apply(varVal)
          body <- body.check(ctx.ext(name, Bind(arg_t)), realRetType)
        } yield Lambda(name, body)
      }
      case _ =>
        Left(Err(
          s"expected ValPi(arg_t, ret_t), found: ${t}"))
    }
}
