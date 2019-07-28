package xieyuheng.cicada

object Ctx {
  type Env = Map[String, Def]
  type Bind = Map[Value, Value]
}

case class Ctx(
  env: Ctx.Env,
  bind: Ctx.Bind)
