package xieyuheng.cicada

import scala.collection.immutable.ListMap

sealed trait Neutral
final case class NeutralVar(name: String) extends Neutral
final case class NeutralCase(target: Neutral, map: ListMap[String, Value]) extends Neutral
final case class NeutralField(target: Neutral, fieldName: String) extends Neutral
final case class NeutralApply(target: Neutral, args: ListMap[String, Value]) extends Neutral
