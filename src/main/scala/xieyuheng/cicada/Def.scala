package xieyuheng.cicada

sealed trait Def

final case class DefineValue(
  name: String,
  value: Value,
) extends Def

final case class DefineMemberType(
  name: String,
  map: MultiMap[String, Exp],
  superName: String,
) extends Def

final case class DefineSumType(
  name: String,
  map: MultiMap[String, Exp],
  memberNames: List[String],
) extends Def

final case class DefineFn(
  name: String,
  args: MultiMap[String, Exp],
  ret: Exp,
  body: Exp,
) extends Def
