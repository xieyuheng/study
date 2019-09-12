package xieyuheng.minitt

sealed trait Norm
final case class NormNeu(neu: NeuNorm) extends Norm
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

sealed trait NeuNorm
final case class NeuNormVar(name: String) extends NeuNorm
final case class NeuNormAp(target: NeuNorm, arg: Val) extends NeuNorm
final case class NeuNormCar(target: NeuNorm) extends NeuNorm
final case class NeuNormCdr(target: NeuNorm) extends NeuNorm
final case class NeuNormMat(target: NeuNorm, mats: Map[String, Exp], env: NormEnv) extends NeuNorm

sealed trait NormEnv
final case class NormEnvDecl(decl: Decl, rest: NormEnv) extends NormEnv
final case class NormEnvPat(pat: Pat, value: Val, rest: NormEnv) extends NormEnv
final case class NormEnvEmpty() extends NormEnv
