package xieyuheng.mini_tt

import xieyuheng.mini_tt.pretty._

object eval {
  def ap(f: Value, arg: Value): Value = {
    f match {
      case FnValue(fnclo) =>
        eval(fnclo.body, PatternEnv(fnclo.pattern, arg, fnclo.env))
      case MatValue(chclo) => arg match {
        case DataValue(tag, body) =>
          chclo.mats.get(tag) match {
            case Some(exp) => ap(eval(exp, chclo.env), body)
            case None => throw new Exception()
          }
        case NeutralValue(target) => NeutralValue(MatNeutral(target, chclo))
        case _ => throw new Exception()
      }
      case NeutralValue(target) => NeutralValue(ApNeutral(target, arg))
      case _ => throw new Exception()
    }
  }

  def car(value: Value): Value = {
    value match {
      case ConsValue(car: Value, cdr: Value) => car
      case NeutralValue(target) => NeutralValue(CarNeutral(target))
      case _ =>
        println(s"value is not a ConsValue: ${prettyValue(value)}")
        throw new Exception()
    }
  }

  def cdr(value: Value): Value = {
    value match {
      case ConsValue(car: Value, cdr: Value) => cdr
      case NeutralValue(target) => NeutralValue(CdrNeutral(target))
      case _ =>
        println(s"value is not a ConsValue: ${prettyValue(value)}")
        throw new Exception()
    }
  }

  def lookup(name: String, env: Env): Value = {
    env match {
      case PatternEnv(pattern, value, rest) =>
        projectPattern(name, pattern, value) match {
          case Some(value) => value
          case None => lookup(name, rest)
        }
      case DeclEnv(Let(pattern, t, e), rest) =>
        projectPattern(name, pattern, eval(e, rest)) match {
          case Some(value) => value
          case None => lookup(name, rest)
        }
      case DeclEnv(Letrec(pattern, t, e), rest) =>
        projectPattern(name, pattern, eval(e, env)) match {
          case Some(value) => value
          case None => lookup(name, rest)
        }
      case EmptyEnv =>
        println(s"can not find name: ${name}")
        throw new Exception()
    }
  }

  def projectPattern(name: String, pattern: Pattern, value: Value): Option[Value] = {
    pattern match {
      case VarPattern(name2) =>
        if (name == name2) {
          Some(value)
        } else {
          None
        }
      case ConsPattern(carPattern: Pattern, cdrPattern: Pattern) =>
        projectPattern(name, carPattern, car(value)) match {
          case Some(value) => Some(value)
          case None => projectPattern(name, cdrPattern, cdr(value)) match {
            case Some(value) => Some(value)
            case None => None
          }
        }
      case SolePattern => None
    }
  }

  def apply(exp: Exp, env: Env): Value = {
    exp match {
      case Var(name) => lookup(name, env)
      case Fn(pattern: Pattern, body: Exp) =>
        FnValue(FnClosure(pattern, body, env))
      case Ap(fun: Exp, arg: Exp) => ap(eval(fun, env), eval(arg, env))
      case Pi(pattern: Pattern, argType: Exp, t: Exp) =>
        PiValue(eval(argType, env), FnClosure(pattern, t, env))
      case Cons(car, cdr) => ConsValue(eval(car, env), eval(cdr, env))
      case Car(pair) => car(eval(pair, env))
      case Cdr(pair) => cdr(eval(pair, env))
      case Sigma(pattern: Pattern, argType: Exp, t: Exp) =>
        SigmaValue(eval(argType, env), FnClosure(pattern, t, env))
      case Data(tag, body) => DataValue(tag, eval(body, env))
      case Mat(mats) => MatValue(MatClosure(mats, env))
      case Sum(mats) => SumValue(MatClosure(mats, env))
      case Sole => SoleValue
      case Trivial => TrivialValue
      case U => UValue
    }
  }
}
