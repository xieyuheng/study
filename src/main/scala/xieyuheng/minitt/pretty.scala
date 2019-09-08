package xieyuheng.minitt

object pretty {

  val IndentUnit: String = "  "

  def getIndent(level: Int): String = {
    assert(level >= 0)
    IndentUnit * level
  }

  def addIndentToBlock(block: String, level: Int): String = {
    block
      .split("\n")
      .map(getIndent(level) ++ _)
      .mkString("\n")
  }

  def maybeNewline(string: String): String = {
    if (string.trim.isEmpty) {
      ""
    } else {
      "\n" ++ addIndentToBlock(string, 1) ++ "\n"
    }
  }


  def prettyExpMapWithDelimiter(
    map: Map[String, Exp],
    delimiter: String,
  ): String = {
    map
      .map { case (name, exp) => s"${name}: ${prettyExp(exp)}" }
      .mkString(delimiter)
  }

  def prettyExpMap(map: Map[String, Exp]): String = {
    prettyExpMapWithDelimiter(map, "\n")
  }

  def prettyExp(exp: Exp): String = {
    exp match {
      case Var(name) => name
      case Fn(pattern, body) =>
        s"${prettyPattern(pattern)} => ${prettyExp(body)}"
      case Ap(fun, arg) =>
        s"${prettyExp(fun)} $$ ${prettyExp(arg)}"
      case Pi(pattern, arg, t) =>
        s"(${prettyPattern(pattern)} : ${prettyExp(arg)}) -> ${prettyExp(t)}"
      case Cons(car, cdr) => s"${prettyExp(car)} * ${prettyExp(cdr)}"
      case Car(pair) => s"${prettyExp(pair)}.car"
      case Cdr(pair) => s"${prettyExp(pair)}.cdr"
      case Sigma(pattern, arg, t) =>
        s"(${prettyPattern(pattern)} : ${prettyExp(arg)}) * ${prettyExp(t)}"
      case Data(tag: String, body: Exp) =>
        s"${tag} ${prettyExp(body)}"
      case Choice(choices: Map[String, Exp]) =>
        s"choice {${prettyExpMap(choices)}}"
      case Sum(choices: Map[String, Exp]) =>
        s"sum {${prettyExpMap(choices)}}"
      case Sole => "()"
      case Trivial => "()"
      case U => "U"
    }
  }

  def prettyPattern(pattern: Pattern): String = {
    pattern match {
      case VarPattern(name) => name
      case ConsPattern(car, cdr) => s"${prettyPattern(car)} * ${prettyPattern(cdr)}"
      case UnderscorePattern => "_"
    }
  }

  def prettyNeutral(neutral: Neutral): String = {
    neutral match {
      case VarNeutral(name: String) =>
        s"[${name}]"
      case ApNeutral(target: Neutral, arg: Value) =>
        s"[${prettyNeutral(target)} => ${prettyValue(arg)}]"
      case CarNeutral(target: Neutral) =>
        s"[${prettyNeutral(target)}.car]"
      case CdrNeutral(target: Neutral) =>
        s"[${prettyNeutral(target)}.cdr]"
      case ChoiceNeutral(target: Neutral, ChoiceClosure(choices: Map[String, Exp], env: Env)) =>
        s"[choice (${prettyNeutral(target)}) {${maybeNewline(prettyExpMap(choices))}}]"
    }
  }

  def prettyValue(value: Value): String = {
    value match {
      case NeutralValue(neutral: Neutral) => prettyNeutral(neutral)
      case FnValue(FnClosure(pattern: Pattern, body: Exp, env: Env)) =>
        s"${prettyPattern(pattern)} => ${prettyExp(body)}"
      case PiValue(arg: Value, FnClosure(pattern: Pattern, body: Exp, env: Env)) =>
        s"(${prettyPattern(pattern)} : ${prettyValue(arg)}) -> ${prettyExp(body)}"
      case SigmaValue(arg: Value, FnClosure(pattern: Pattern, body: Exp, env: Env)) =>
        s"(${prettyPattern(pattern)} : ${prettyValue(arg)}) * ${prettyExp(body)}"
      case UValue => "U"
      case ConsValue(car: Value, cdr: Value) =>
        s"${prettyValue(car)} * ${prettyValue(cdr)}"
      case SoleValue => "()"
      case TrivialValue => "()"
      case DataValue(tag: String, body: Value) =>
        s"${tag} ${prettyValue(body)}"
      case SumValue(ChoiceClosure(choices: Map[String, Exp], env: Env)) =>
        s"sum {${maybeNewline(prettyExpMap(choices))}}"
      case ChoiceValue(ChoiceClosure(choices: Map[String, Exp], env: Env)) =>
        s"choice {${maybeNewline(prettyExpMap(choices))}}"
    }
  }

}
