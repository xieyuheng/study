package xieyuheng.cicada

sealed trait Norm
final case class NormType(level: Int) extends Norm
// final case class NormPi(arg_name: String, arg_t: Norm, dep_t: Clo) extends Norm
// final case class NormFn(arg_name: String, arg_t: Norm, body: Clo) extends Norm
// final case class NormClub(name: String, members: List[Member], tel: Telescope) extends Norm
// final case class NormMember(name: String, club_name: String, tel: Telescope) extends Norm
// final case class NormRecord(name: String, super_names: List[String], tel: Telescope) extends Norm


sealed trait NormNeu extends Norm
final case class NormNeuVar(name: String) extends NormNeu
// TODO

sealed trait NormEnv
// TODO
