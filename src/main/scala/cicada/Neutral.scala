package cicada

import scala.collection.immutable.ListMap

sealed trait Neutral
final case class VarNeutral(name: String) extends Neutral
final case class CaseNeutral(target: Neutral, map: ListMap[String, Value]) extends Neutral
final case class FieldNeutral(target: Neutral, fieldName: String) extends Neutral
final case class ApNeutral(target: Neutral, args: ListMap[String, Value]) extends Neutral
