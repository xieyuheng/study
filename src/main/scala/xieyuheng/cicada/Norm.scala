package xieyuheng.cicada

sealed trait Norm
final case class NormType(level: Int) extends Norm
final case class NormPi(arg_name: String, arg_t: Norm, dep_t: Norm) extends Norm
final case class NormFn(arg_name: String, arg_t: Norm, body: Norm) extends Norm
final case class NormClub(name: String, members: List[Member], tel: NormTelescope) extends Norm
final case class NormMember(name: String, club_name: String, tel: NormTelescope) extends Norm
final case class NormRecord(name: String, super_names: List[String], telNorm: NormTelescope) extends Norm

case class NormTelescope(
  fields: List[(String, Exp, Option[Exp], Option[Norm], Option[Norm])],
  env: NormEnv)

sealed trait NormNeu extends Norm
final case class NormNeuVar(name: String) extends NormNeu
final case class NormNeuAp(target: NormNeu, arg: Norm) extends NormNeu
final case class NormNeuChoice(target: NormNeu, map: Map[String, Exp], env: NormEnv) extends NormNeu
final case class NormNeuDot(target: NormNeu, field_name: String) extends NormNeu
final case class NormNeuDotType(target: NormNeu, field_name: String) extends NormNeu

sealed trait NormEnv
final case class NormEnvDecl(decl: Decl, rest: NormEnv) extends NormEnv
final case class NormEnvVal(name: String, value: Norm, rest: NormEnv) extends NormEnv
final case class NormEnvEmpty() extends NormEnv

object NormEnv {
  def apply(): NormEnv = NormEnvEmpty()
}
