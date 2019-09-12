package xieyuheng.minitt

import xieyuheng.minitt.pretty._

object eval {

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
      case PatternEnv(pattern, value, rest) =>
        project_pattern(name, pattern, value) match {
          case Some(value) => value
          case None => lookup(name, rest)
        }
      case DeclEnv(Let(pattern, t, e), rest) =>
        project_pattern(name, pattern, eval(e, rest)) match {
          case Some(value) => value
          case None => lookup(name, rest)
        }
      case DeclEnv(Letrec(pattern, t, e), rest) =>
        project_pattern(name, pattern, eval(e, env)) match {
          case Some(value) => value
          case None => lookup(name, rest)
        }
      case EmptyEnv() =>
        println(s"can not find name: ${name}")
        throw new Exception()
    }
  }

  def project_pattern(name: String, pattern: Pattern, value: Val): Option[Val] = {
    pattern match {
      case VarPattern(name2) =>
        if (name == name2) {
          Some(value)
        } else {
          None
        }
      case ConsPattern(carPattern: Pattern, cdrPattern: Pattern) =>
        project_pattern(name, carPattern, car(value)) match {
          case Some(value) => Some(value)
          case None => project_pattern(name, cdrPattern, cdr(value)) match {
            case Some(value) => Some(value)
            case None => None
          }
        }
      case SolePattern() => None
    }
  }

  def apply(exp: Exp, env: Env): Val = {
    exp match {
      case Var(name) => lookup(name, env)
      case Fn(pattern: Pattern, body: Exp) =>
        ValFn(CloFn(pattern, body, env))
      case Ap(fn: Exp, arg: Exp) => exe.ap_val(eval(fn, env), eval(arg, env))
      case Pi(pattern: Pattern, arg_t: Exp, t: Exp) =>
        ValPi(eval(arg_t, env), CloFn(pattern, t, env))
      case Cons(car, cdr) => ValCons(eval(car, env), eval(cdr, env))
      case Car(pair) => car(eval(pair, env))
      case Cdr(pair) => cdr(eval(pair, env))
      case Sigma(pattern: Pattern, arg_t: Exp, t: Exp) =>
        ValSigma(eval(arg_t, env), CloFn(pattern, t, env))
      case Data(tag, body) => ValData(tag, eval(body, env))
      case Mat(mats) => ValMat(CloMat(mats, env))
      case Sum(mats) => ValSum(CloMat(mats, env))
      case Sole() => ValSole()
      case Trivial() => ValTrivial()
      case Univ() => ValUniv()
    }
  }
}
