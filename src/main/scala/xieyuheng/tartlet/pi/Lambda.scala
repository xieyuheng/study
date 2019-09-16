package xieyuheng.tartlet

import java.util.UUID

case class Fn (
  name: String,
  body: Exp,
) extends Constructor {
  def eval(env: Env): Either[Err, Val] =
    Right(ValFn(EnvClo(env, name, body)))

  def alpha_eq(
    that: Exp,
    this_map: Map[String, String],
    that_map: Map[String, String],
  ): Boolean = {
    that match {
      case Fn(name2, body2) => {
        val sym = UUID.randomUUID().toString
        body.alpha_eq(body2, this_map + (name -> sym), that_map + (name2 -> sym))
      }
      case _ => false
    }
  }

  /*
    ctx.ext(x, A) :- body <= R
    ------------------------------
    ctx :- Fn(x, body) <= Pi
   */
  def check(ctx: Ctx, t: Val): Either[Err, Exp] =
    t match {
      case ValPi(arg_t, ret_t) => {
        val varVal = TheNeu(arg_t, NeuVar(name))
        for {
          realRetType <- ret_t.apply(varVal)
          body <- body.check(ctx.ext(name, Bind(arg_t)), realRetType)
        } yield Fn(name, body)
      }
      case _ =>
        Left(Err(
          s"expected ValPi(arg_t, ret_t), found: ${t}"))
    }
}
