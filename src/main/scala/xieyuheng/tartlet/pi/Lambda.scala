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
      case ValPi(argType, retType) => {
        val varVal = TheNeu(argType, NeuVar(name))
        for {
          realRetType <- retType.apply(varVal)
          body <- body.check(ctx.ext(name, Bind(argType)), realRetType)
        } yield Lambda(name, body)
      }
      case _ =>
        Left(Err(
          s"expected ValPi(argType, retType), found: ${t}"))
    }
}
