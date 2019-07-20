package xieyuheng.cicada

case class Ctx(
  env: Map[String, Value],
  typeEnv: Map[String, Value],
  uniEnv: Map[Value, Value])
