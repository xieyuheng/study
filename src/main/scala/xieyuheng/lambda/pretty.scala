package xieyuheng.lambda

object pretty {

  val INDENT_UNIT: String = "  "

  def get_indent(level: Int): String = {
    assert(level >= 0)
    INDENT_UNIT * level
  }

  def add_indent_to_block(block: String, level: Int): String = {
    block
      .split("\n")
      .map(get_indent(level) ++ _)
      .mkString("\n")
  }

  def maybeln(string: String): String = {
    if (string.trim.isEmpty) {
      ""
    } else {
      "\n" ++ add_indent_to_block(string, 1) ++ "\n"
    }
  }


  def pretty_exp_map_with_delimiter(
    map: Map[String, Exp],
    delimiter: String,
  ): String = {
    map
      .map { case (name, exp) => s"${name}: ${pretty_exp(exp)}" }
      .mkString(delimiter)
  }

  def pretty_exp_map(map: Map[String, Exp]): String = {
    pretty_exp_map_with_delimiter(map, ";\n")
  }

  def pretty_exp(exp: Exp): String = {
    exp match {
      case Var(name) => name
      case Fn(name, body) =>
        s"${name} => ${pretty_exp(body)}"
      case Ap(fn, arg) =>
        s"${pretty_exp(fn)}(${pretty_exp(arg)})"
      case Block(decl, body) =>
        s"{ ${pretty_decl(decl)}; ${pretty_exp(body)} }"
    }
  }

  def pretty_decl(decl: Decl): String = {
    decl match {
      case DeclLet(name, e) =>
        s"let ${name} = ${pretty_exp(e)}"
    }
  }

  def pretty_neu(neu: Neu): String = {
    neu match {
      case NeuVar(name: String) =>
        name
      case NeuAp(target: Neu, arg: Val) =>
        s"${pretty_neu(target)}(${pretty_val(arg)})"
    }
  }

  def pretty_val(value: Val): String = {
    value match {
      case neu: Neu => pretty_neu(neu)
      case ValFn(name, body, env) =>
        // val map_str = pretty_env(env)
        // s"${name} => ${pretty_exp(body)} #env {${maybeln(map_str)}}"
        s"${name} => ${pretty_exp(body)}"
    }
  }

  def pretty_env(env: Env): String = {
    val delimiter = ";\n"
    env.map
      .map { case (name, value) => s"${name}: ${pretty_val(value)}" }
      .mkString(delimiter)
  }
}
