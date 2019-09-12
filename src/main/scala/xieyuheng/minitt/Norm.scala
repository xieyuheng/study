package xieyuheng.minitt

sealed trait Norm
final case class NormFn(name: String, body: Norm) extends Norm
final case class NormPi(name: String, arg_t: Norm, t: Norm) extends Norm
final case class NormSigma(name: String, arg_t: Norm, t: Norm) extends Norm
final case class NormCons(car: Norm, cdr: Norm) extends Norm
final case class NormData(tag: String, body: Norm) extends Norm
final case class NormMat(mats: Map[String, Exp], env: NormEnv) extends Norm
final case class NormSum(mats: Map[String, Exp], env: NormEnv) extends Norm
final case class NormSole() extends Norm
final case class NormTrivial() extends Norm
final case class NormUniv() extends Norm

sealed trait NormNeu extends Norm
final case class NormNeuVar(name: String) extends NormNeu
final case class NormNeuAp(target: NormNeu, arg: Norm) extends NormNeu
final case class NormNeuCar(target: NormNeu) extends NormNeu
final case class NormNeuCdr(target: NormNeu) extends NormNeu
final case class NormNeuMat(target: NormNeu, mats: Map[String, Exp], env: NormEnv) extends NormNeu

sealed trait NormEnv
final case class NormEnvDecl(decl: Decl, rest: NormEnv) extends NormEnv
final case class NormEnvPat(pat: Pat, value: Norm, rest: NormEnv) extends NormEnv
final case class NormEnvEmpty() extends NormEnv
