package xieyuheng.cicada

sealed trait Exp
final case class Var(name: String) extends Exp
final case class Type() extends Exp
final case class Pi(arg_name: String, arg_t: Exp, dep_t: Exp) extends Exp
final case class Fn(arg_name: String, arg_t: Exp, dep_t: Exp, body: Exp) extends Exp
final case class Ap(target: Exp, arg: Exp) extends Exp
final case class Club(name: String, members: List[Member], fields: List[(String, Exp, Option[Exp])]) extends Exp
final case class Member(name: String, club_name: String, fields: List[(String, Exp, Option[Exp])]) extends Exp
final case class Choice(path: List[String], map: Map[String, Exp]) extends Exp
final case class Dot(target: Exp, field_name: String) extends Exp
final case class Let(decl: Decl, body: Exp) extends Exp
