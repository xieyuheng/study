package xieyuheng.cicada

import scala.collection.immutable.ListMap

sealed trait Value
final case class TypeValue(uuid: String) extends Value
final case class UnionValue(name: String, map: ListMap[String, Value], subNames: List[String]) extends Value
final case class RecordValue(name: String, map: ListMap[String, Value]) extends Value
final case class PiValue(args: ListMap[String, Value], ret: Value) extends Value
final case class FnValue(args: ListMap[String, Value], ret: Value, body: Exp, env: Map[String, Value]) extends Value
final case class NeutralValue(neutral: Neutral) extends Value
