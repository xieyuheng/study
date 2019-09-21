package xieyuheng.cicada

sealed trait Val
final case class ValType(level: Int) extends Val
// final case class ValThe(t: Val, body: Val) extends Val
final case class ValPi(arg_name: String, arg_t: Val, dep_t: Clo) extends Val
final case class ValFn(arg_name: String, arg_t: Val, body: Clo) extends Val
final case class ValClub(name: String, members: List[Member], tel: Telescope) extends Val
final case class ValMember(name: String, club_name: String, tel: Telescope) extends Val
final case class ValRecord(name: String, super_names: List[String], tel: Telescope) extends Val

case class Clo(arg_name: String, body: Exp, env: Env) {
  def apply(arg: Val): Val = {
    eval(body, env.ext(arg_name, arg))
  }
}

case class Telescope(
  fileds: List[(String, Exp, Option[Exp])],
  fills: List[(String, Val, Val)],
  env: Env,
) {
  def put(arg: Val): Telescope = {
    ???
  }

  def dot(field_name: String): Val = {
    ???
  }

  def dot_type(field_name: String): Val = {
    ???
  }
}

sealed trait Neu extends Val
final case class NeuVar(name: String) extends Neu
final case class NeuAp(target: Neu, arg: Val) extends Neu
final case class NeuChoice(target: Neu, map: Map[String, Val]) extends Neu
final case class NeuDot(target: Neu, field_name: String) extends Neu
final case class NeuDotType(target: Neu, field_name: String) extends Neu
