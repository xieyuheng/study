package xieyuheng.cicada

sealed trait Value

final case class TypeVar(
  id: Id,
) extends Value

final case class UnionValue(
  id: Id,
  name: String,
  map: MultiMap[String, Value],
  memberNames: List[String],
  bind: Bind,
) extends Value

final case class RecordValue(
  name: String,
  map: MultiMap[String, Value],
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
