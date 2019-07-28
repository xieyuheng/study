package xieyuheng.cicada

case class Ctx(
  declEnv: Map[String, Decl],
  typeEnv: Map[String, Value],
  unifEnv: Map[Value, Value])
