package xieyuheng.cicada

case class Ctx(
  declEnv: Map[String, Value],
  typeEnv: Map[String, Value],
  unifEnv: Map[Value, Value])
