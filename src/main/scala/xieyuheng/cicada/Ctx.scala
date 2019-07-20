package xieyuheng.cicada

case class Ctx(
  env: Map[String, Exp],
  typeEnv: Map[String, Exp],
  uniEnv: Map[Value, Value])
