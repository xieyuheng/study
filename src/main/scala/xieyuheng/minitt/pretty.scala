package xieyuheng.minitt

import xieyuheng.util.pretty._

object pretty {

  def pretty_exp_map(map: Map[String, Exp]) =
    pretty_map(map) {
      case (name, exp) =>
        s"${name}: ${pretty_exp(exp)};\n" }

  def pretty_exp_case(map: Map[String, Exp]) =
    pretty_map(map) {
      case (name, exp) =>
        s"case ${name} => ${pretty_exp(exp)}\n" }

  def cons_exp_to_list(cons: Cons): List[Exp] = {
    cons.cdr match {
      case cdr: Cons => cons.car :: cons_exp_to_list(cdr)
      case _ => cons.car :: List(cons.cdr)
    }
  }

  def pretty_exp(exp: Exp): String = {
    exp match {
      case Var(name) => name
      case Fn(pat, body) =>
        s"${pretty_pat(pat)} => ${pretty_exp(body)}"
      case Ap(fn, arg) =>
        s"${pretty_exp(fn)}(${pretty_exp(arg)})"
      case Pi(pat, arg_t, t) =>
        s"(${pretty_pat(pat)}: ${pretty_exp(arg_t)}) -> ${pretty_exp(t)}"
      case cons: Cons =>
        val str = cons_exp_to_list(cons).map(pretty_exp(_)).mkString(", ")
        s"[${str}]"
      case Car(pair) => s"car(${pretty_exp(pair)})"
      case Cdr(pair) => s"cdr(${pretty_exp(pair)})"
      case Sigma(pat, arg_t, t) =>
        s"(${pretty_pat(pat)}: ${pretty_exp(arg_t)}) * ${pretty_exp(t)}"
      case Data(tag: String, body: Cons) =>
        s"${tag}${pretty_exp(body)}"
      case Data(tag: String, body: Sole) =>
        s"${tag}${pretty_exp(body)}"
      case Data(tag: String, body: Exp) =>
        s"${tag}[${pretty_exp(body)}]"
      case Mat(mats: Map[String, Exp]) =>
        s"{${maybe_ln(pretty_exp_case(mats))}}"
      case Sum(mats: Map[String, Exp]) =>
        s"datatype {${maybe_ln(pretty_exp_map(mats))}}"
      case Sole() => "[]"
      case Trivial() => "[]"
      case Univ() => "type_t"
      case Block(decl, body) =>
        s"{ ${pretty_decl(decl)}; ${pretty_exp(body)} }"
    }
  }

  def pretty_decl(decl: Decl): String = {
    decl match {
      case DeclLet(pat: Pat, t: Exp, e: Exp) =>
        s"let ${pretty_pat(pat)}: ${pretty_exp(t)} = ${pretty_exp(e)}"
      case DeclLetrec(pat: Pat, t: Exp, e: Exp) =>
        s"let rec ${pretty_pat(pat)}: ${pretty_exp(t)} = ${pretty_exp(e)}"
    }
  }

  def pretty_pat(pat: Pat): String = {
    pat match {
      case PatVar(name) => name
      case PatCons(car, cdr) => s"${pretty_pat(car)} * ${pretty_pat(cdr)}"
      case PatSole() => "[]"
    }
  }

  def pretty_neu(neu: Neu): String = {
    neu match {
      case NeuVar(name: String, aka: Option[String]) =>
        aka match {
          case Some(alias) => s"${alias}${name}"
          case None => name
        }
      case NeuAp(target: Neu, arg: Val) =>
        s"${pretty_neu(target)}(${pretty_val(arg)})"
      case NeuCar(target: Neu) =>
        s"car(${pretty_neu(target)})"
      case NeuCdr(target: Neu) =>
        s"cdr(${pretty_neu(target)})"
      case NeuMat(target: Neu, CloMat(mats: Map[String, Exp], env: Env)) =>
        s"choice (${pretty_neu(target)}) {${maybe_ln(pretty_exp_case(mats))}}"
    }
  }

  def cons_val_to_list(cons: ValCons): List[Val] = {
    cons.cdr match {
      case cdr: ValCons => cons.car :: cons_val_to_list(cdr)
      case _ => cons.car :: List(cons.cdr)
    }
  }


  def pretty_clo(clo: Clo): String = {
    clo match {
      case CloFn(pat: Pat, body: Exp, env: Env) =>
        s"(${pretty_pat(pat)}) => ${pretty_exp(body)}"
      case CloMat(mats, env: Env) =>
        s"{${maybe_ln(pretty_exp_case(mats))}}"
      case CloTag(tag: String, clo: Clo) =>
        s"${pretty_clo(clo)} on ${tag}"
    }
  }

  def pretty_val(value: Val): String = {
    value match {
      case ValNeu(neu: Neu) => pretty_neu(neu)
      case ValFn(CloFn(pat: Pat, body: Exp, env: Env)) =>
        s"${pretty_pat(pat)} => ${pretty_exp(body)}"
      case ValPi(arg: Val, clo) =>
        s"(${pretty_val(arg)}) -> ${pretty_clo(clo)}"
      case ValSigma(arg: Val, clo) =>
        s"(${pretty_val(arg)}) * ${pretty_clo(clo)}"
      case ValUniv() => "type_t"
      case cons: ValCons =>
        val str = cons_val_to_list(cons).map(pretty_val(_)).mkString(", ")
        s"[${str}]"
      case ValSole() => "[]"
      case ValTrivial() => "[]"
      case ValData(tag: String, body: ValCons) =>
        s"${tag}${pretty_val(body)}"
      case ValData(tag: String, body: ValSole) =>
        s"${tag}${pretty_val(body)}"
      case ValData(tag: String, body: Val) =>
        s"${tag}[${pretty_val(body)}]"
      case ValSum(CloMat(mats: Map[String, Exp], env: Env)) =>
        s"datatype {${maybe_ln(pretty_exp_map(mats))}}"
      case ValMat(CloMat(mats: Map[String, Exp], env: Env)) =>
        s"{${maybe_ln(pretty_exp_case(mats))}}"
    }
  }

}
