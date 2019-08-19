package xieyuheng.cicada

sealed trait Neutral
final case class VarNeutral(name: String) extends Neutral
final case class CaseNeutral(target: Neutral, map: MultiMap[String, Value]) extends Neutral
final case class FieldNeutral(target: Neutral, fieldName: String) extends Neutral
final case class ApNeutral(target: Neutral, args: MultiMap[String, Value]) extends Neutral
