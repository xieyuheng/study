package xieyuheng.minitt

sealed trait Val
final case class ValNeu(neu: Neu) extends Val
final case class ValFn(clo_fn: CloFn) extends Val
final case class ValPi(arg_t: Val, clo_fn: CloFn) extends Val
final case class ValSigma(arg_t: Val, clo_fn: CloFn) extends Val
final case class ValUniv() extends Val
final case class ValCons(car: Val, cdr: Val) extends Val
final case class ValSole() extends Val
final case class ValTrivial() extends Val
final case class ValData(tag: String, body: Val) extends Val
final case class ValSum(clo_mat: CloMat) extends Val
final case class ValMat(clo_mat: CloMat) extends Val

sealed trait Clo
final case class CloFn(pattern: Pattern, body: Exp, env: Env) extends Clo
final case class CloMat(mats: Map[String, Exp], env: Env) extends Clo

sealed trait Neu
final case class NeuVar(name: String) extends Neu
final case class NeuAp(target: Neu, arg: Val) extends Neu
final case class NeuCar(target: Neu) extends Neu
final case class NeuCdr(target: Neu) extends Neu
final case class NeuMat(target: Neu, clo_mat: CloMat) extends Neu
