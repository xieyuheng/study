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

  def check(exp: Exp, t: Value) = {
    ???
    // - target must be Union,
    //   and clauses.keys == target.subNames
    // - this means we should generate a eliminator
    //   (like `fold`) for every Union
    //   (but we are using unification and `.field`
    //   instead of pattern matching)

    //   foreach name and body in clauses {
    //     sub <- ctx.loopupValue(name)
    //     ctx :- target :> sub
    //     ctx.fulfill(target, sub) :- body <: t
    //   }
    //   --------------------
    //   ctx :- Case(target, clauses) <: t

    // - target can be Record or Union

    //   ctx :- target.map.get(fieldName) <: t
    //   ----------------
    //   ctx :- Field(target, fieldName) <: t

    // - Apply

    //   fun can be Fn or Pi
    //   ctx :- args <: target.args
    //   ctx.fulfill(args, target.args) :- target.ret <: t
    //   --------------------
    //   ctx :- Apply(target, args) <: t
  }
}
