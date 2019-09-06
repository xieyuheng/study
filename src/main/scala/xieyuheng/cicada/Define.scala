package xieyuheng.cicada

sealed trait Define

final case class DefineValue(
  name: String,
  value: Value,
) extends Define

final case class DefineMemberType(
  name: String,
  map: MultiMap[String, Exp],
  superName: String,
) extends Define

final case class DefineSumType(
  name: String,
  map: MultiMap[String, Exp],
  memberNames: List[String],
) extends Define

final case class DefineFn(
  name: String,
  args: MultiMap[String, Exp],
  ret: Exp,
  body: Exp,
) extends Define
