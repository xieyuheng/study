package xieyuheng..tartlet

trait Exp {
  def eval(env: Env): Either[ErrorMsg, Value]

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean

  def infer(ctx: Ctx): Either[ErrorMsg, The]

  def check(ctx: Ctx, t: Value): Either[ErrorMsg, Exp]
}
