package xieyuheng.tartlet

trait Exp {
  def eval(env: Env): Either[Err, Val]

  def alpha_eq(
    that: Exp,
    this_map: Map[String, String],
    that_map: Map[String, String],
  ): Boolean

  def infer(ctx: Ctx): Either[Err, The]

  def check(ctx: Ctx, t: Val): Either[Err, Exp]
}
