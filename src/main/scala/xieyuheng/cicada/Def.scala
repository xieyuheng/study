package xieyuheng.cicada

import scala.collection.immutable.ListMap

sealed trait Def

final case class DefineValue(
  name: String,
  value: Value,
) extends Def

final case class DefineRecord(
  name: String,
  map: ListMap[String, Exp],
) extends Def

final case class DefineUnion(
  name: String,
  map: ListMap[String, Exp],
  subNames: List[String],
) extends Def
