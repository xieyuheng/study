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
    } yield ValSigma(carTypeVal, EnvClosure(env, name, cdrType))
  }

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case Sigma(name2, carType2, cdrType2) => {
        val sym = UUID.randomUUID().toString
        carType.alphaEq(carType2, thisMap, thatMap) &&
        cdrType.alphaEq(cdrType2, thisMap + (name -> sym), thatMap + (name -> sym))
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
      carTypeVal <- carType.eval(ctx.toEnv)
      cdrType <- cdrType.check(ctx.ext(name, Bind(carTypeVal)), ValUniverse)
    } yield The(Universe, Pi(name, carType, cdrType))
  }
}
