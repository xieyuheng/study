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
        println(s"[eval fail]")
        println(s"value is not a ValCons: ${prettyVal(value)}")
        throw new Exception()
    }
  }

  def cdr(value: Val): Val = {
    value match {
      case ValCons(car: Val, cdr: Val) => cdr
      case ValNeu(target) => ValNeu(NeuCdr(target))
      case _ =>
        println(s"[eval fail]")
        println(s"value is not a ValCons: ${prettyVal(value)}")
        throw new Exception()
    }
  }

  def lookup(name: String, env: Env): Val = {
    env match {
      case EnvPat(pat, value, rest) =>
        pat_proj(pat, name, value) match {
          case Some(value) => value
          case None => lookup(name, rest)
        }
      case EnvDecl(DeclLet(pat, t, e), rest) =>
        pat_proj(pat, name, eval(e, rest)) match {
          case Some(value) => value
          case None => lookup(name, rest)
        }
      case EnvDecl(DeclLetrec(pat, t, e), rest) =>
        pat_proj(pat, name, eval(e, env)) match {
          case Some(value) => value
          case None => lookup(name, rest)
        }
      case EnvEmpty() =>
        println(s"[eval fail]")
        println(s"can not find name: ${name}")
        throw new Exception()
    }
  }

  def pat_proj(pat: Pat, name: String, value: Val): Option[Val] = {
    pat match {
      case PatVar(name2) =>
        if (name == name2) {
          Some(value)
        } else {
          None
        }
      case PatCons(car_pat: Pat, cdr_pat: Pat) =>
        pat_proj(car_pat, name, car(value)) match {
          case Some(value) => Some(value)
          case None => pat_proj(cdr_pat, name, cdr(value)) match {
            case Some(value) => Some(value)
            case None => None
          }
        }
      case PatSole() => None
    }
  }

}
