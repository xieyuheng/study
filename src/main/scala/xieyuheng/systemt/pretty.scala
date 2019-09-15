package xieyuheng.systemt

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

  def prettyExp(exp: Exp): String = {
    s"${exp}"
    // exp match {
    //   case Var(name) => name
    //   case Fn(name, body) =>
    //     s"${name} => ${prettyExp(body)}"
    //   case Ap(fn, arg) =>
    //     s"${prettyExp(fn)}(${prettyExp(arg)})"
    //   case Block(decl, body) =>
    //     s"{ ${prettyDecl(decl)}; ${prettyExp(body)} }"
    // }
  }

  def prettyDecl(decl: Decl): String = {
    decl match {
      case DeclLet(name, e) =>
        s"let ${name} = ${prettyExp(e)}"
    }
  }

  // def prettyNeu(neu: Neu): String = {
  //   neu match {
  //     case NeuVar(name: String) =>
  //       name
  //     case NeuAp(target: Neu, arg: Val) =>
  //       s"${prettyNeu(target)}(${prettyVal(arg)})"
  //   }
  // }

  def prettyVal(value: Val): String = {
    s"${value}"
    // value match {
    //   case neu: Neu => prettyNeu(neu)
    //   case ValFn(name, body, env) =>
    //     // val map_str = prettyEnv(env)
    //     // s"${name} => ${prettyExp(body)} #env {${maybeNewline(map_str)}}"
    //     s"${name} => ${prettyExp(body)}"
    // }
  }

  // def prettyEnv(env: Env): String = {
  //   val delimiter = ";\n"
  //   env.map
  //     .map { case (name, value) => s"${name}: ${prettyVal(value)}" }
  //     .mkString(delimiter)
  // }
}
