package xieyuheng.cicada

case class Ctx(
  declEnv: Map[String, Decl],
  unifMap: Map[Value, Value])
