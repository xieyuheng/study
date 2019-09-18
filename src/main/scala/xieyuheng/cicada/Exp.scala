package xieyuheng.cicada

import scala.collection.immutable.ListMap

sealed trait Exp

final case class Var(name: String) extends Exp

final case class Type() extends Exp

final case class Pi(arg_name: String, arg_t: Exp, dep_t: Exp) extends Exp
final case class Fn(arg_name: String, arg_t: Exp, dep_t: Exp, body: Exp) extends Exp
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
  club_name: String,
) extends Exp

final case class Record(
  name: String,
  fileds: ListMap[String, (Exp, Option[Exp])],
  super_names: List[String],
) extends Exp

final case class GetField(target: Exp, field_name: String) extends Exp
final case class GetFieldType(target: Exp, field_name: String) extends Exp
