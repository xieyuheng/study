package xieyuheng.cicada

sealed trait Exp

final case class Var(name: String) extends Exp

final case class Union(name: String, map: Map[String, Exp], subNames: List[String]) extends Exp
final case class Case(target: Exp, map: Map[String, Exp]) extends Exp

final case class Record(name: String, map: Map[String, Exp]) extends Exp
final case class Field(target: Exp, fieldName: String) extends Exp

final case class Pi(args: Map[String, Exp], ret: Exp) extends Exp
final case class Fn(args: Map[String, Exp], ret: Exp, body: Exp) extends Exp
final case class Apply(target: Exp, args: Map[String, Exp]) extends Exp


sealed trait Value
sealed trait Neutral extends Value

final case class VarNeutral(uuid: String, name: String) extends Neutral

final case class UnionValue(name: String, map: Map[String, Value], subNames: List[String]) extends Value
final case class CaseNeutral(target: Neutral, map: Map[String, Value]) extends Neutral

final case class RecordValue(name: String, map: Map[String, Value]) extends Value
final case class FieldNeutral(target: Neutral, fieldName: String) extends Neutral

final case class PiValue(args: Map[String, Value], ret: Value) extends Value
final case class FnValue(args: Map[String, Value], ret: Value, body: Exp) extends Value
final case class ApplyNeutral(target: Neutral, args: Map[String, Value]) extends Neutral
