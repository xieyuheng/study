package xieyuheng.cicada

sealed trait Exp
final case class Var(name: String) extends Exp
final case class Type() extends Exp
final case class Union(name: String, map: Map[String, Exp], subNames: List[String]) extends Exp
final case class Case(target: Exp, map: Map[String, Exp]) extends Exp
final case class Record(name: String, map: Map[String, Exp]) extends Exp
final case class Field(target: Exp, fieldName: String) extends Exp
final case class Pi(args: Map[String, Exp], ret: Exp) extends Exp
final case class Fn(args: Map[String, Exp], ret: Exp, body: Exp) extends Exp
final case class Apply(target: Exp, args: Map[String, Exp]) extends Exp
