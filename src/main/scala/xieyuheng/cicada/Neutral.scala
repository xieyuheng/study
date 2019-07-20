package xieyuheng.cicada

sealed trait Neutral
final case class NeutralVar(name: String) extends Neutral
final case class NeutralCase(target: Neutral, map: Map[String, Value]) extends Neutral
final case class NeutralField(target: Neutral, fieldName: String) extends Neutral
final case class NeutralApply(target: Neutral, args: Map[String, Value]) extends Neutral
