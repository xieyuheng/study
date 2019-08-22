package xieyuheng..tartlet

import java.util.UUID

case class Pi (
  name: String,
  argType: Exp,
  retType: Exp,
) extends Type {
  def eval(env: Env): Either[ErrorMsg, Value] = {
    for {
      argTypeValue <- argType.eval(env)
    } yield ValuePi(argTypeValue, EnvClosure(env, name, retType))
  }

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case Pi(name2, argType2, retType2) => {
        val sym = UUID.randomUUID().toString
        argType.alphaEq(argType2, thisMap, thatMap) &&
        retType.alphaEq(retType2, thisMap + (name -> sym), thatMap + (name -> sym))
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
  def infer(ctx: Ctx): Either[ErrorMsg, The] = {
    for {
      argType <- argType.check(ctx, ValueUniverse)
      argTypeValue <- argType.eval(ctx.toEnv)
      retType <- retType.check(ctx.ext(name, Bind(argTypeValue)), ValueUniverse)
    } yield The(Universe, Pi(name, argType, retType))
  }
}
