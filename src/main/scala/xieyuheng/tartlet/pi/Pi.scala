package xieyuheng.tartlet

import java.util.UUID

case class Pi (
  name: String,
  arg_t: Exp,
  ret_t: Exp,
) extends Type {
  def eval(env: Env): Either[Err, Val] = {
    for {
      arg_tVal <- arg_t.eval(env)
    } yield ValPi(arg_tVal, EnvClo(env, name, ret_t))
  }

  def alpha_eq(
    that: Exp,
    this_map: Map[String, String],
    that_map: Map[String, String],
  ): Boolean = {
    that match {
      case Pi(name2, arg_t2, ret_t2) => {
        val sym = UUID.randomUUID().toString
        arg_t.alpha_eq(arg_t2, this_map, that_map) &&
        ret_t.alpha_eq(ret_t2, this_map + (name -> sym), that_map + (name -> sym))
      }
      case _ => false
    }
  }

  /*
   ctx :- A <= Universe
   ctx.ext(x, A) :- B <= Universe
   -----------------
   ctx :- Pi(x: A, B) => Universe
   */
  def infer(ctx: Ctx): Either[Err, The] = {
    for {
      arg_t <- arg_t.check(ctx, ValUniverse)
      arg_tVal <- arg_t.eval(ctx.to_env)
      ret_t <- ret_t.check(ctx.ext(name, Bind(arg_tVal)), ValUniverse)
    } yield The(Universe, Pi(name, arg_t, ret_t))
  }
}
