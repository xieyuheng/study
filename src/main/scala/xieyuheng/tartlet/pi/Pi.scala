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

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case Pi(name2, arg_t2, ret_t2) => {
        val sym = UUID.randomUUID().toString
        arg_t.alphaEq(arg_t2, thisMap, thatMap) &&
        ret_t.alphaEq(ret_t2, thisMap + (name -> sym), thatMap + (name -> sym))
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
      arg_tVal <- arg_t.eval(ctx.toEnv)
      ret_t <- ret_t.check(ctx.ext(name, Bind(arg_tVal)), ValUniverse)
    } yield The(Universe, Pi(name, arg_t, ret_t))
  }
}
