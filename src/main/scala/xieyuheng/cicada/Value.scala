package xieyuheng.cicada

sealed trait Value
final case class ValueType(uuid: String) extends Value
final case class ValueUnion(name: String, map: Map[String, Value], subNames: List[String]) extends Value
final case class ValueRecord(name: String, map: Map[String, Value]) extends Value
final case class ValuePi(args: Map[String, Value], ret: Value) extends Value
final case class ValueFn(args: Map[String, Value], ret: Value, body: Exp, env: Map[String, Value]) extends Value
