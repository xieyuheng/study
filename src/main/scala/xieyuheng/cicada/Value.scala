package xieyuheng.cicada

sealed trait Value

final case class TypeOfType(
  id: Id,
) extends Value

final case class SumTypeValue(
  id: Id,
  name: String,
  map: MultiMap[String, Value],
  memberNames: List[String],
  bind: Bind,
) extends Value

final case class MemberTypeValue(
  name: String,
  map: MultiMap[String, Value],
  superName: String,
  bind: Bind,
) extends Value

final case class PiValue(
  id: Id,
  args: MultiMap[String, Value],
  ret: Value,
) extends Value

final case class FnValue(
  args: MultiMap[String, Value],
  ret: Value,
  body: Exp,
  env: Env,
) extends Value

final case class NeutralValue(
  neutral: Neutral,
) extends Value
