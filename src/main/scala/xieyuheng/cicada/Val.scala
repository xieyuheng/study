package xieyuheng.cicada

sealed trait Val
final case class ValType() extends Val
final case class ValVar(id: Id, name: String, t: Val) extends Val
// final case class ValPi(arg_name: String, arg_t: Val, dep_t: Clo) extends Val
// final case class ValFn(arg_name: String, arg_t: Val, dep_t: Clo, body: Clo) extends Val
// final case class ValClub(name: String, members: List[Member], tel: Tel) extends Val
// final case class ValMember(name: String, club_name: String, tel: Tel) extends Val
// final case class ValRecord(name: String, super_names: List[String], tel: Tel) extends Val

sealed trait Neu extends Val
final case class NeuAp(target: Neu, arg: Val) extends Neu
// final case class NeuChoice(target: Neu, path: List[String], map: Map[String, Exp], env: Env) extends Neu
final case class NeuDot(target: Neu, field_name: String) extends Neu
