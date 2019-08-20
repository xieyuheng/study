package xieyuheng.cicada

sealed trait Def

final case class DefineValue(
  name: String,
  value: Value,
) extends Def

final case class DefineRecord(
  name: String,
  map: MultiMap[String, Exp],
) extends Def

final case class DefineUnion(
  name: String,
  map: MultiMap[String, Exp],
  memberNames: List[String],
) extends Def
