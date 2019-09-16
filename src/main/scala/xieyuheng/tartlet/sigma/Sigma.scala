package xieyuheng.tartlet

import java.util.UUID

case class Sigma (
  name: String,
  carType: Exp,
  cdrType: Exp,
) extends Type {
  def eval(env: Env): Either[Err, Val] = {
    for {
      carTypeVal <- carType.eval(env)
    } yield ValSigma(carTypeVal, EnvClo(env, name, cdrType))
  }

  def alpha_eq(
    that: Exp,
    this_map: Map[String, String],
    that_map: Map[String, String],
  ): Boolean = {
    that match {
      case Sigma(name2, carType2, cdrType2) => {
        val sym = UUID.randomUUID().toString
        carType.alpha_eq(carType2, this_map, that_map) &&
        cdrType.alpha_eq(cdrType2, this_map + (name -> sym), that_map + (name -> sym))
      }
      case _ => false
    }
  }

  /*
   ctx :- A <= Universe
   ctx.ext(x, A) :- B <= Universe
   -----------------
   ctx :- Sigma(x: A, B) => Universe
   */
  def infer(ctx: Ctx): Either[Err, The] = {
    for {
      carType <- carType.check(ctx, ValUniverse)
      carTypeVal <- carType.eval(ctx.to_env)
      cdrType <- cdrType.check(ctx.ext(name, Bind(carTypeVal)), ValUniverse)
    } yield The(Universe, Pi(name, carType, cdrType))
  }
}
