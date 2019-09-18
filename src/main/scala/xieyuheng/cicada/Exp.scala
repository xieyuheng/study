package xieyuheng.cicada

import scala.collection.immutable.ListMap

sealed trait Exp

final case class Var(name: String) extends Exp

final case class Type() extends Exp

final case class Pi(argName: String, arg_t: Exp, dep_t: Exp) extends Exp
final case class Fn(argName: String, arg_t: Exp, dep_t: Exp, body: Exp) extends Exp
final case class Ap(target: Exp, arg: Exp) extends Exp

final case class Club(
  name: String,
  fileds: ListMap[String, (Exp, Option[Exp])],
  members: List[Member],
) extends Exp
final case class Choice(target: Exp, map: Map[String, Exp]) extends Exp

final case class Member(
  name: String,
  fileds: ListMap[String, (Exp, Option[Exp])],
  clubName: String,
) extends Exp

final case class Record(
  name: String,
  fileds: ListMap[String, (Exp, Option[Exp])],
  superNames: List[String],
) extends Exp

final case class GetField(target: Exp, fieldName: String) extends Exp
final case class GetFieldType(target: Exp, fieldName: String) extends Exp
