package xieyuheng.cicada

sealed trait Decl

final case class DeclLet(
  name: String,
  t: Exp,
  body: Exp,
) extends Decl

final case class DeclFn(
  name: String,
  args: Map[String, Exp],
  dep_t: Exp,
  body: Exp,
) extends Decl

final case class DeclClub(
  name: String,
  members: List[Member],
  fileds: List[(String, Exp, Option[Exp])],
) extends Decl

case class Member(
  name: String,
  club_name: String,
  fileds: List[(String, Exp, Option[Exp])])

final case class DeclRecord(
  name: String,
  super_names: List[String],
  fileds: List[(String, Exp, Option[Exp])],
) extends Decl
