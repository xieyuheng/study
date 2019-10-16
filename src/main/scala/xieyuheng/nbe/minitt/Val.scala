package xieyuheng.minitt

import xieyuheng.minitt.pretty._

sealed trait Val
final case class ValNeu(neu: Neu) extends Val
final case class ValFn(clo_fn: CloFn) extends Val
final case class ValPi(arg_t: Val, clo: Clo) extends Val
final case class ValSigma(arg_t: Val, clo: Clo) extends Val
final case class ValUniv() extends Val
final case class ValCons(car: Val, cdr: Val) extends Val
final case class ValSole() extends Val
final case class ValTrivial() extends Val
final case class ValData(tag: String, body: Val) extends Val
final case class ValSum(clo_mat: CloMat) extends Val
final case class ValMat(clo_mat: CloMat) extends Val

sealed trait Clo {
  def ap(arg: Val): Val = {
    this match {
      case CloFn(pat: Pat, body: Exp, env: Env) =>
        eval(body, EnvPat(pat, arg, env))
      case clo_mat @ CloMat(mats: Map[String, Exp], env: Env) =>
        arg match {
          case ValData(tag, body) =>
            mats.get(tag) match {
              case Some(exp) =>
                eval.ap(eval(exp, env), body)
              case None =>
                throw new Exception()
            }
          case ValNeu(target) => ValNeu(NeuMat(target, clo_mat))
          case _ => throw new Exception()
        }
      case CloTag(tag: String, clo: Clo) =>
        clo.ap(ValData(tag, arg))
    }
  }

  def maybe_arg_name(): Option[String] = {
    this match {
      case CloFn(pat, _body, _env) => pat.maybe_name()
      case _ => None
    }
  }
}

final case class CloFn(pat: Pat, body: Exp, env: Env) extends Clo
final case class CloMat(mats: Map[String, Exp], env: Env) extends Clo
final case class CloTag(tag: String, clo: Clo) extends Clo

sealed trait Neu
final case class NeuVar(name: String, aka: Option[String] = None) extends Neu {
  val matters = name

  override def equals(that: Any): Boolean = {
    that match {
      case that: NeuVar => this.matters == that.matters
      case _ => false
    }
  }

  override def hashCode = matters.hashCode
}
final case class NeuAp(target: Neu, arg: Val) extends Neu
final case class NeuCar(target: Neu) extends Neu
final case class NeuCdr(target: Neu) extends Neu
final case class NeuMat(target: Neu, clo_mat: CloMat) extends Neu
