package xieyuheng.tartlet

import xieyuheng.util.pretty._

object pretty {

  def pretty_exp_map(map: Map[String, Exp]) =
    pretty_map(map) {
      case (name, exp) =>
        s"${name}: ${pretty_exp(exp)};" }

  def pretty_exp(exp: Exp): String = {
    exp match {
      case Var(name) =>
        name
      case Atom() =>
        s"string_t"
      case Quote(sym: String) =>
        '"' + s"${sym}" + '"'
      case Eqv(t: Exp, from: Exp, to: Exp) =>
        s"eqv_t(${pretty_exp(t)}, ${pretty_exp(from)}, ${pretty_exp(to)})"
      case Same() =>
        s"same"
      case Replace(target: Exp, motive: Exp, base: Exp) =>
        s"replace(${pretty_exp(target)}, ${pretty_exp(motive)}, ${pretty_exp(base)})"
      case Nat() =>
        s"nat_t"
      case Zero() =>
        s"zero"
      case Succ(prev: Exp) =>
        s"succ(${pretty_exp(prev)})"
      case NatInd(target: Exp, motive: Exp, base: Exp, step: Exp) =>
        s"nat_ind(${pretty_exp(target)}, ${pretty_exp(motive)}, ${pretty_exp(base)}, ${pretty_exp(step)})"
      case Pi(name: String, arg_t: Exp, dep_t: Exp) =>
        s"(${name}, ${pretty_exp(arg_t)}) -> ${pretty_exp(dep_t)}"
      case Fn(name, body) =>
        s"${name} => ${pretty_exp(body)}"
      case Ap(fn, arg) =>
        s"${pretty_exp(fn)}(${pretty_exp(arg)})"
      case Absurd() =>
        s"absurd_t"
      case AbsurdInd(target: Exp, motive: Exp) =>
        s"absurd_ind(${pretty_exp(target)}, ${pretty_exp(motive)})"
      case Sigma(name: String, arg_t: Exp, dep_t: Exp) =>
        s"(${name}, ${pretty_exp(arg_t)}) * ${pretty_exp(dep_t)}"
      case Cons(car: Exp, cdr: Exp) =>
        s"cons(${pretty_exp(car)}, ${pretty_exp(cdr)})"
      case Car(pair: Exp) =>
        s"car(${pretty_exp(pair)})"
      case Cdr(pair: Exp) =>
        s"cdr(${pretty_exp(pair)})"
      case Sole() =>
        s"sole"
      case Trivial() =>
        s"trivial_t"
      case Universe() =>
        s"type_t"
      case The(t: Exp, e: Exp) =>
        s"the(${pretty_exp(t)}, ${pretty_exp(e)})"
        // case Block(decl, body) =>
        //   s"{ ${pretty_decl(decl)}; ${pretty_exp(body)} }"
    }
  }

  def pretty_decl(decl: Decl): String = {
    decl match {
      case DeclLet(name, t, e) =>
        s"let ${name}: ${pretty_exp(t)} = ${pretty_exp(e)}"
    }
  }

  def pretty_neu(neu: Neu): String = {
    neu match {
      case NeuVar(name: String) =>
        name
      case NeuReplace(target: Neu, motive: TheVal, base: TheVal) =>
        s"replace(${pretty_neu(target)}, ${pretty_the_val(motive)}, ${pretty_the_val(base)})"
      case NeuNatInd(target: Neu, motive: TheVal, base: TheVal, step: TheVal) =>
        s"nat_rec(${pretty_neu(target)}, ${pretty_the_val(motive)}, ${pretty_the_val(base)}, ${pretty_the_val(step)})"
      case NeuAbsurdInd(target: Neu, motive: TheVal) =>
        s"absurd_ind(${pretty_neu(target)}, ${pretty_the_val(motive)})"
      case NeuAp(target: Neu, arg: TheVal) =>
        s"${pretty_neu(target)}(${pretty_the_val(arg)})"
      case NeuCar(pair: Neu) =>
        s"car(${pretty_neu(pair)})"
      case NeuCdr(pair: Neu) =>
        s"cdr(${pretty_neu(pair)})"
    }
  }

  def pretty_the_val(the: TheVal): String = {
    s"the(${pretty_val(the.t)}, ${pretty_val(the.value)})"
  }

  def pretty_clo(clo: Clo): String = {
    clo match {
      case CloNative(name, fn) =>
        s"${name} => ${fn}"
      case CloEnv(env, name, body) =>
        s"${name} => ${pretty_exp(body)}"
    }
  }

  def pretty_val(value: Val): String = {
    value match {
      case TheNeu(t: Val, neu: Neu) =>
        s"the(${pretty_val(t)}, ${pretty_neu(neu)})"
      case ValAbsurd() =>
        s"absurd_t"
      case ValAtom() =>
        s"string_t"
      case ValQuote(sym: String) =>
        '"' + s"${sym}" + '"'
      case ValEqv(t: Val, from: Val, to: Val) =>
        s"eqv_t(${pretty_val(t)}, ${pretty_val(from)}, ${pretty_val(to)})"
      case ValSame() =>
        s"same"
      case ValNat() =>
        s"nat_t"
      case ValZero() =>
        s"zero"
      case ValSucc(prev: Val) =>
        s"succ(${pretty_val(prev)})"
      case ValPi(arg_t: Val, dep_t: Clo) =>
        s"(${pretty_val(arg_t)}) -> ${pretty_clo(dep_t)}"
      case ValFn(clo: Clo) =>
        s"${pretty_clo(clo)}"
      case ValSigma(arg_t: Val, dep_t: Clo) =>
        s"(${pretty_val(arg_t)}) * ${pretty_clo(dep_t)}"
      case ValCons(car: Val, cdr: Val) =>
        s"cons(${pretty_val(car)}, ${pretty_val(cdr)})"
      case ValSole() =>
        s"sole"
      case ValTrivial() =>
        s"trivial_t"
      case ValUniverse() =>
        s"type_t"
    }
  }
}
