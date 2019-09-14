package xieyuheng.tartlet

trait Exp {
  def eval(env: Env): Either[Err, Val]

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean

  def infer(ctx: Ctx): Either[Err, The]

  def check(ctx: Ctx, t: Val): Either[Err, Exp]
}
