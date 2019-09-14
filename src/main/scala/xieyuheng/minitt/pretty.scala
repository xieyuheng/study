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
      case Fn(pat, body) =>
        s"${prettyPat(pat)} => ${prettyExp(body)}"
      case Ap(fn, arg) =>
        s"${prettyExp(fn)}(${prettyExp(arg)})"
      case Pi(pat, arg_t, t) =>
        s"(${prettyPat(pat)}: ${prettyExp(arg_t)}) -> ${prettyExp(t)}"
      case cons: Cons =>
        val str = cons_exp_to_list(cons).map(prettyExp(_)).mkString(", ")
        s"[${str}]"
      case Car(pair) => s"car(${prettyExp(pair)})"
      case Cdr(pair) => s"cdr(${prettyExp(pair)})"
      case Sigma(pat, arg_t, t) =>
        s"(${prettyPat(pat)}: ${prettyExp(arg_t)}) ** ${prettyExp(t)}"
      case Data(tag: String, body: Cons) =>
        s"${tag}${prettyExp(body)}"
      case Data(tag: String, body: Sole) =>
        s"${tag}${prettyExp(body)}"
      case Data(tag: String, body: Exp) =>
        s"${tag}[${prettyExp(body)}]"
      case Mat(mats: Map[String, Exp]) =>
        s"match {${maybeNewline(prettyExpMap(mats))}}"
      case Sum(mats: Map[String, Exp]) =>
        s"sum {${maybeNewline(prettyExpMap(mats))}}"
      case Sole() => "[]"
      case Trivial() => "[]"
      case Univ() => "univ"
      case Block(decl, body) =>
        s"{ ${prettyDecl(decl)}; ${prettyExp(body)} }"
    }
  }

  def prettyDecl(decl: Decl): String = {
    decl match {
      case DeclLet(pat: Pat, t: Exp, e: Exp) =>
        s"let ${prettyPat(pat)}: ${prettyExp(t)} = ${prettyExp(e)}"
      case DeclLetrec(pat: Pat, t: Exp, e: Exp) =>
        s"letrec ${prettyPat(pat)}: ${prettyExp(t)} = ${prettyExp(e)}"
    }
  }

  def prettyPat(pat: Pat): String = {
    pat match {
      case PatVar(name) => name
      case PatCons(car, cdr) => s"${prettyPat(car)} * ${prettyPat(cdr)}"
      case PatSole() => "[]"
    }
  }

  def prettyNeu(neu: Neu): String = {
    neu match {
      case NeuVar(name: String) =>
        s"#[${name}]"
      case NeuAp(target: Neu, arg: Val) =>
        s"#[${prettyNeu(target)}(${prettyVal(arg)})]"
      case NeuCar(target: Neu) =>
        s"#[car(${prettyNeu(target)})]"
      case NeuCdr(target: Neu) =>
        s"#[cdr(${prettyNeu(target)})]"
      case NeuMat(target: Neu, CloMat(mats: Map[String, Exp], env: Env)) =>
        s"#[match {${maybeNewline(prettyExpMap(mats))}} (${prettyNeu(target)})]"
    }
  }

  def cons_val_to_list(cons: ValCons): List[Val] = {
    cons.cdr match {
      case cdr: ValCons => cons.car :: cons_val_to_list(cdr)
      case _ => cons.car :: List(cons.cdr)
    }
  }


  def prettyClo(clo: Clo): String = {
    clo match {
      case CloFn(pat: Pat, body: Exp, env: Env) =>
        s"(${prettyPat(pat)}) => ${prettyExp(body)}"
      case CloMat(mats, env: Env) =>
        s"match {${maybeNewline(prettyExpMap(mats))}}"
      case CloTag(tag: String, clo: Clo) =>
        s"${prettyClo(clo)} on ${tag}"
    }
  }

  def prettyVal(value: Val): String = {
    value match {
      case ValNeu(neu: Neu) => prettyNeu(neu)
      case ValFn(CloFn(pat: Pat, body: Exp, env: Env)) =>
        s"${prettyPat(pat)} => ${prettyExp(body)}"
      case ValPi(arg: Val, clo) =>
        s"(${prettyVal(arg)}) -> ${prettyClo(clo)}"
      case ValSigma(arg: Val, clo) =>
        s"(${prettyVal(arg)}) ** ${prettyClo(clo)}"
      case ValUniv() => "univ"
      case cons: ValCons =>
        val str = cons_val_to_list(cons).map(prettyVal(_)).mkString(", ")
        s"[${str}]"
      case ValSole() => "[]"
      case ValTrivial() => "[]"
      case ValData(tag: String, body: ValCons) =>
        s"${tag}${prettyVal(body)}"
      case ValData(tag: String, body: ValSole) =>
        s"${tag}${prettyVal(body)}"
      case ValData(tag: String, body: Val) =>
        s"${tag}[${prettyVal(body)}]"
      case ValSum(CloMat(mats: Map[String, Exp], env: Env)) =>
        s"sum {${maybeNewline(prettyExpMap(mats))}}"
      case ValMat(CloMat(mats: Map[String, Exp], env: Env)) =>
        s"match {${maybeNewline(prettyExpMap(mats))}}"
    }
  }

}
