package xieyuheng.minitt

object readback {

  def readback_value(i: Int, value: Value): Value = {
    value match {
      case NeutralValue(neutral: Neutral) => NeutralValue(readback_neutral(i, neutral))
      case FnValue(fnclo: FnClosure) => ???
      case PiValue(arg: Value, fnclo: FnClosure) => ???
      case SigmaValue(arg: Value, fnclo: FnClosure) => ???
      case UValue => ???
      case ConsValue(car: Value, cdr: Value) => ???
      case SoleValue => ???
      case TrivialValue => ???
      case DataValue(tag: String, body: Value) => ???
      case SumValue(chclo: MatClosure) => ???
      case MatValue(chclo: MatClosure) => ???
    }
  }

  def readback_neutral(i: Int, neutral: Neutral): Neutral = {
    neutral match {
      case VarNeutral(name: String) => ???
      case ApNeutral(target: Neutral, arg: Value) => ???
      case CarNeutral(target: Neutral) => ???
      case CdrNeutral(target: Neutral) => ???
      case MatNeutral(target: Neutral, chclo: MatClosure) => ???
    }
  }

  def readback_env(i: Int, env: Env): Env = {
    env match {
      case DeclEnv(decl: Decl, rest: Env) => ???
      case PatternEnv(pattern: Pattern, value: Value, rest: Env) => ???
      case EmptyEnv => ???
    }
  }
}
