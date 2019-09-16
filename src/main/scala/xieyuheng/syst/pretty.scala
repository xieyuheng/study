package xieyuheng.syst

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
      case The(t: Type, e: Exp) =>
        s"the(${pretty_type(t)}, ${pretty_exp(e)})"
      case Zero() =>
        s"zero"
      case Succ(prev: Exp) =>
        s"succ(${pretty_exp(prev)})"
      case NatRec(t: Type, target: Exp, base: Exp, step: Exp) =>
        s"nat_rec(${pretty_type(t)}, ${pretty_exp(target)}, ${pretty_exp(base)}, ${pretty_exp(step)})"
      case Block(decl, body) =>
        s"{ ${pretty_decl(decl)}; ${pretty_exp(body)} }"
    }
  }

  def pretty_type(t: Type): String = {
    t match {
      case Nat() =>
        s"nat_t"
      case Arrow(arg_t: Type, ret_t: Type) =>
        s"(${pretty_type(arg_t)}) -> ${pretty_type(ret_t)}"
    }
  }

  def pretty_decl(decl: Decl): String = {
    decl match {
      case DeclLet(name, t, e) =>
        s"let ${name}: ${pretty_type(t)} = ${pretty_exp(e)}"
    }
  }

  def pretty_neu(neu: Neu): String = {
    neu match {
      case NeuVar(name: String) =>
        name
      case NeuAp(target: Neu, arg: TheVal) =>
        s"${pretty_neu(target)}(${pretty_the_val(arg)})"
      case NeuNatRec(t: Type, target: Neu, base: TheVal, step: TheVal) =>
        s"nat_rec(${pretty_type(t)}, ${pretty_neu(target)}, ${pretty_the_val(base)}, ${pretty_the_val(step)})"
    }
  }

  def pretty_the_val(the: TheVal): String = {
    s"the(${pretty_type(the.t)}, ${pretty_val(the.value)})"
  }

  def pretty_val(value: Val): String = {
    value match {
      case TheNeu(t: Type, neu: Neu) =>
        s"the(${pretty_type(t)}, ${pretty_neu(neu)})"
      case ValSucc(prev: Val) =>
        s"succ(${pretty_val(prev)})"
      case ValZero() =>
        s"zero"
      case ValFn(name, body, env) =>
        // val map_str = pretty_env(env)
        // s"${name} => ${pretty_exp(body)} #env {${maybeln(map_str)}}"
        s"${name} => ${pretty_exp(body)}"
    }
  }

  // def pretty_env(env: Env): String = {
  //   val delimiter = ";\n"
  //   env.map
  //     .map { case (name, value) => s"${name}: ${pretty_val(value)}" }
  //     .mkString(delimiter)
  // }
}
