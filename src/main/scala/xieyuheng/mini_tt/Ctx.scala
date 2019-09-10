package xieyuheng.mini_tt

sealed trait Ctx
final case class PatternCtx(pattern: Pattern, value: Value, rest: Ctx) extends Ctx
final case object EmptyCtx extends Ctx

object Ctx {
  def extend(pattern: Pattern, value: Value, rest: Ctx): Ctx = {
    // TODO
    PatternCtx(pattern: Pattern, value: Value, rest: Ctx)
  }
}
