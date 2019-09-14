package xieyuheng.cicada.with_logic_variable

import scala.collection.immutable.ListMap

sealed trait Val

final case class TypeOfType(
  id: Id,
) extends Val

final case class ValOfType(
  id: Id,
  t: Val,
) extends Val

final case class SumTypeVal(
  name: String,
  map: ListMap[String, Val],
  memberNames: List[String],
  bind: Bind,
) extends Val

final case class MemberTypeVal(
  name: String,
  map: ListMap[String, Val],
  superName: String,
  bind: Bind,
) extends Val

final case class PiVal(
  args: ListMap[String, Val],
  ret: Val,
) extends Val

final case class FnVal(
  args: ListMap[String, Val],
  ret: Val,
  body: Exp,
  env: Env,
) extends Val

final case class NeuVal(
  neutral: Neu,
) extends Val

final case class TopVal() extends Val

final case class BottomVal() extends Val
