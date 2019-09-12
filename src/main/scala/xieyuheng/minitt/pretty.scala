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
    prettyExpMapWithDelimiter(map, ";\n")
  }

  def cons_exp_to_list(cons: Cons): List[Exp] = {
    cons.cdr match {
      case cdr: Cons => cons.car :: cons_exp_to_list(cdr)
      case _ => cons.car :: List(cons.cdr)
    }
  }

  def prettyExp(exp: Exp): String = {
    exp match {
      case Var(name) => name
      case Fn(pattern, body) =>
        s"${prettyPattern(pattern)} => ${prettyExp(body)}"
      case Ap(fn, arg) =>
        s"${prettyExp(fn)}(${prettyExp(arg)})"
      case Pi(pattern, argType, t) =>
        s"(${prettyPattern(pattern)}: ${prettyExp(argType)}) -> ${prettyExp(t)}"
      case cons: Cons =>
        val str = cons_exp_to_list(cons).map(prettyExp(_)).mkString(", ")
        s"[${str}]"
      case Car(pair) => s"${prettyExp(pair)}.car"
      case Cdr(pair) => s"${prettyExp(pair)}.cdr"
      case Sigma(pattern, argType, t) =>
        s"(${prettyPattern(pattern)}: ${prettyExp(argType)}) ** ${prettyExp(t)}"
      case Data(tag: String, body: Cons) =>
        s"${tag}${prettyExp(body)}"
      case Data(tag: String, body: Sole.type) =>
        s"${tag}${prettyExp(body)}"
      case Data(tag: String, body: Exp) =>
        s"${tag}[${prettyExp(body)}]"
      case Mat(mats: Map[String, Exp]) =>
        s"match {${prettyExpMap(mats)}}"
      case Sum(mats: Map[String, Exp]) =>
        s"sum {${prettyExpMap(mats)}}"
      case Sole => "[]"
      case Trivial => "[]"
      case U => "U"
    }
  }

  def prettyPattern(pattern: Pattern): String = {
    pattern match {
      case VarPattern(name) => name
      case ConsPattern(car, cdr) => s"${prettyPattern(car)} * ${prettyPattern(cdr)}"
      case SolePattern => "[]"
    }
  }

  def prettyNeutral(neutral: Neutral): String = {
    neutral match {
      case VarNeutral(name: String) =>
        s"[${name}]"
      case ApNeutral(target: Neutral, arg: Value) =>
        s"[${prettyNeutral(target)}(${prettyValue(arg)})]"
      case CarNeutral(target: Neutral) =>
        s"[${prettyNeutral(target)}.car]"
      case CdrNeutral(target: Neutral) =>
        s"[${prettyNeutral(target)}.cdr]"
      case MatNeutral(target: Neutral, MatClosure(mats: Map[String, Exp], env: Env)) =>
        s"[match {${maybeNewline(prettyExpMap(mats))}} (${prettyNeutral(target)})]"
    }
  }

  def cons_value_to_list(cons: ConsValue): List[Value] = {
    cons.cdr match {
      case cdr: ConsValue => cons.car :: cons_value_to_list(cdr)
      case _ => cons.car :: List(cons.cdr)
    }
  }

  def prettyValue(value: Value): String = {
    value match {
      case NeutralValue(neutral: Neutral) => prettyNeutral(neutral)
      case FnValue(FnClosure(pattern: Pattern, body: Exp, env: Env)) =>
        s"${prettyPattern(pattern)} => ${prettyExp(body)}"
      case PiValue(arg: Value, FnClosure(pattern: Pattern, body: Exp, env: Env)) =>
        s"(${prettyPattern(pattern)}: ${prettyValue(arg)}) -> ${prettyExp(body)}"
      case SigmaValue(arg: Value, FnClosure(pattern: Pattern, body: Exp, env: Env)) =>
        s"(${prettyPattern(pattern)}: ${prettyValue(arg)}) ** ${prettyExp(body)}"
      case UValue => "U"
      case cons: ConsValue =>
        val str = cons_value_to_list(cons).map(prettyValue(_)).mkString(", ")
        s"[${str}]"
      case SoleValue => "[]"
      case TrivialValue => "[]"
      case DataValue(tag: String, body: ConsValue) =>
        s"${tag}${prettyValue(body)}"
      case DataValue(tag: String, body: SoleValue.type) =>
        s"${tag}${prettyValue(body)}"
      case DataValue(tag: String, body: Value) =>
        s"${tag}[${prettyValue(body)}]"
      case SumValue(MatClosure(mats: Map[String, Exp], env: Env)) =>
        s"sum {${maybeNewline(prettyExpMap(mats))}}"
      case MatValue(MatClosure(mats: Map[String, Exp], env: Env)) =>
        s"match {${maybeNewline(prettyExpMap(mats))}}"
    }
  }

}
