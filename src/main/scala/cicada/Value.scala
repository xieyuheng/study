package cicada

import scala.collection.immutable.ListMap

sealed trait Value

final case class TypeOfType(
  id: Id,
) extends Value

final case class ValueOfType(
  id: Id,
  t: Value,
) extends Value

final case class SumTypeValue(
  name: String,
  map: ListMap[String, Value],
  memberNames: List[String],
  bind: Bind,
) extends Value

final case class MemberTypeValue(
  name: String,
  map: ListMap[String, Value],
  superName: String,
  bind: Bind,
) extends Value

final case class PiValue(
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
