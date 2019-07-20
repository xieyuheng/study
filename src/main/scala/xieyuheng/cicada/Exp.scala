package xieyuheng.cicada

import scala.collection.immutable.ListMap

sealed trait Exp
final case class Var(name: String) extends Exp
final case class Type() extends Exp
final case class Union(name: String, map: ListMap[String, Exp], subNames: List[String]) extends Exp
final case class Case(target: Exp, map: ListMap[String, Exp]) extends Exp
final case class Record(name: String, map: ListMap[String, Exp]) extends Exp
final case class Field(target: Exp, fieldName: String) extends Exp
final case class Pi(args: ListMap[String, Exp], ret: Exp) extends Exp
final case class Fn(args: ListMap[String, Exp], ret: Exp, body: Exp) extends Exp
final case class Apply(target: Exp, args: ListMap[String, Exp]) extends Exp

object Exp {
  def eval(exp: Exp, env: Map[String, Value]): Either[ErrorMessage, Value] = {
    exp match {
      case _ => ???
    }
  }

  def check(exp: Exp) = {
    ???
  }
}
