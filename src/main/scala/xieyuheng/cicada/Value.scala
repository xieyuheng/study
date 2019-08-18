package xieyuheng.cicada

import scala.collection.immutable.ListMap

sealed trait Value

final case class LogicVar(
  id: Id,
) extends Value

final case class UnionValue(
  id: Id,
  name: String,
  map: ListMap[String, Value],
  subNames: List[String],
  bind: Bind,
) extends Value

final case class RecordValue(
  name: String,
  map: ListMap[String, Value],
  bind: Bind,
) extends Value

final case class PiValue(
  id: Id,
  args: ListMap[String, Value],
  ret: Value,
) extends Value

final case class FnValue(
  args: ListMap[String, Value],
  ret: Value,
  body: Exp,
  env: Env,
) extends Value

final case class NeutralValue(
  neutral: Neutral,
) extends Value
