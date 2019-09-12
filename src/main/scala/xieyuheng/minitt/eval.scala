package xieyuheng.minitt

import xieyuheng.minitt.pretty._

object eval {
  def apply_closure(closure: Closure, arg: Value): Value = {
    closure match {
      case FnClosure(pattern: Pattern, body: Exp, env: Env) =>
        eval(body, PatternEnv(pattern, arg, env))
      case mat_closure @ MatClosure(mats: Map[String, Exp], env: Env) =>
        arg match {
          case DataValue(tag, body) =>
            mats.get(tag) match {
              case Some(exp) => ap(eval(exp, env), body)
              case None => throw new Exception()
            }
          case NeutralValue(target) => NeutralValue(MatNeutral(target, mat_closure))
          case _ => throw new Exception()
        }
    }
  }

  def ap(f: Value, arg: Value): Value = {
    f match {
      case FnValue(closure) => apply_closure(closure, arg)
      case MatValue(closure) => apply_closure(closure, arg)
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
      case EmptyEnv =>
        println(s"can not find name: ${name}")
        throw new Exception()
    }
  }

  def project_pattern(name: String, pattern: Pattern, value: Value): Option[Value] = {
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
      case SolePattern => None
    }
  }

  def apply(exp: Exp, env: Env): Value = {
    exp match {
      case Var(name) => lookup(name, env)
      case Fn(pattern: Pattern, body: Exp) =>
        FnValue(FnClosure(pattern, body, env))
      case Ap(fn: Exp, arg: Exp) => ap(eval(fn, env), eval(arg, env))
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
