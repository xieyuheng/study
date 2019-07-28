package xieyuheng.cicada

case class Ctx(
  declEnv: Map[String, Decl],
  unifEnv: Map[Value, Value])
