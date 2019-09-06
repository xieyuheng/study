package xieyuheng.cicada

import scala.collection.immutable.ListMap

sealed trait Neutral
final case class VarNeutral(name: String) extends Neutral
final case class ChoiceNeutral(target: Neutral, map: ListMap[String, Value]) extends Neutral
final case class DotNeutral(target: Neutral, fieldName: String) extends Neutral
final case class ApNeutral(target: Neutral, args: ListMap[String, Value]) extends Neutral
