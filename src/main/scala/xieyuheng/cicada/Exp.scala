package xieyuheng.cicada

import scala.collection.immutable.ListMap

sealed trait Exp
final case class Var(name: String) extends Exp
final case class Type() extends Exp
final case class Case(target: Exp, map: ListMap[String, Exp]) extends Exp
final case class Field(target: Exp, fieldName: String) extends Exp
final case class Pi(args: ListMap[String, Exp], ret: Exp) extends Exp
final case class Fn(args: ListMap[String, Exp], ret: Exp, body: Exp) extends Exp
final case class Apply(target: Exp, args: ListMap[String, Exp]) extends Exp
