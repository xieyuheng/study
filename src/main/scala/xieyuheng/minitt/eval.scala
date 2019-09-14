package xieyuheng.minitt

import xieyuheng.minitt.pretty._

object eval {

  def apply(exp: Exp, env: Env): Val = {
    exp match {
      case Var(name) => lookup(name, env)
      case Fn(pat: Pat, body: Exp) =>
        ValFn(CloFn(pat, body, env))
      case Ap(fn: Exp, arg: Exp) => ap(eval(fn, env), eval(arg, env))
      case Pi(pat: Pat, arg_t: Exp, dep_t: Exp) =>
        ValPi(eval(arg_t, env), CloFn(pat, dep_t, env))
      case Sigma(pat: Pat, arg_t: Exp, dep_t: Exp) =>
        ValSigma(eval(arg_t, env), CloFn(pat, dep_t, env))
      case Cons(car, cdr) => ValCons(eval(car, env), eval(cdr, env))
      case Car(pair) => car(eval(pair, env))
      case Cdr(pair) => cdr(eval(pair, env))
      case Data(tag, body) => ValData(tag, eval(body, env))
      case Mat(mats) => ValMat(CloMat(mats, env))
      case Sum(mats) => ValSum(CloMat(mats, env))
      case Sole() => ValSole()
      case Trivial() => ValTrivial()
      case Univ() => ValUniv()
      case Block(decl, body) =>
        eval(body, EnvDecl(decl, env))
    }
  }

  def ap(f: Val, arg: Val): Val = {
    f match {
      case ValFn(clo) => clo.ap(arg)
      case ValMat(clo) => clo.ap(arg)
      case ValNeu(target) => ValNeu(NeuAp(target, arg))
      case _ => throw new Exception()
    }
  }

  def car(value: Val): Val = {
    value match {
      case ValCons(car: Val, cdr: Val) => car
      case ValNeu(target) => ValNeu(NeuCar(target))
      case _ =>
        println(s"value is not a ValCons: ${prettyVal(value)}")
        throw new Exception()
    }
  }

  def cdr(value: Val): Val = {
    value match {
      case ValCons(car: Val, cdr: Val) => cdr
      case ValNeu(target) => ValNeu(NeuCdr(target))
      case _ =>
        println(s"value is not a ValCons: ${prettyVal(value)}")
        throw new Exception()
    }
  }

  def lookup(name: String, env: Env): Val = {
    env match {
      case EnvPat(pat, value, rest) =>
        project_pat(name, pat, value) match {
          case Some(value) => value
          case None => lookup(name, rest)
        }
      case EnvDecl(DeclLet(pat, t, e), rest) =>
        project_pat(name, pat, eval(e, rest)) match {
          case Some(value) => value
          case None => lookup(name, rest)
        }
      case EnvDecl(DeclLetrec(pat, t, e), rest) =>
        project_pat(name, pat, eval(e, env)) match {
          case Some(value) => value
          case None => lookup(name, rest)
        }
      case EnvEmpty() =>
        println(s"can not find name: ${name}")
        throw new Exception()
    }
  }

  def project_pat(name: String, pat: Pat, value: Val): Option[Val] = {
    pat match {
      case PatVar(name2) =>
        if (name == name2) {
          Some(value)
        } else {
          None
        }
      case PatCons(carPat: Pat, cdrPat: Pat) =>
        project_pat(name, carPat, car(value)) match {
          case Some(value) => Some(value)
          case None => project_pat(name, cdrPat, cdr(value)) match {
            case Some(value) => Some(value)
            case None => None
          }
        }
      case PatSole() => None
    }
  }

}
