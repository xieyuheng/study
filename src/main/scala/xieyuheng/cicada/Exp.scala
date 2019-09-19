package xieyuheng.cicada

sealed trait Exp
final case class Var(name: String) extends Exp
final case class Type(level: Int) extends Exp
final case class The(t: Exp, body: Exp) extends Exp
final case class Pi(arg_name: String, arg_t: Exp, dep_t: Exp) extends Exp
final case class Fn(arg_name: String, arg_t: Exp, dep_t: Exp, body: Exp) extends Exp
final case class Ap(target: Exp, arg: Exp) extends Exp

final case class Club(
  name: String,
  member_names: List[String],
  fileds: List[(String, Exp, Option[Exp])],
) extends Exp

final case class Member(
  name: String,
  club_name: String,
  fileds: List[(String, Exp, Option[Exp])],
) extends Exp

final case class Record(
  name: String,
  super_names: List[String],
  fileds: List[(String, Exp, Option[Exp])],
) extends Exp

final case class Choice(target: Exp, map: Map[String, Exp]) extends Exp
final case class Dot(target: Exp, field_name: String) extends Exp
final case class DotType(target: Exp, field_name: String) extends Exp
