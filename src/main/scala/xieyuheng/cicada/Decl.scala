package xieyuheng.cicada

sealed trait Decl {
  def name: String
}

final case class DeclLet(name: String, t: Exp, body: Exp) extends Decl
final case class DeclFn(name: String, args: List[(String, Exp)], dep_t: Exp, body: Exp) extends Decl
final case class DeclClub(name: String, members: List[Member], fields: List[(String, Exp, Option[Exp])]) extends Decl
