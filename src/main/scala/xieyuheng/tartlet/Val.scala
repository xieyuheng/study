package xieyuheng.tartlet

trait Val
case object ValAbsurd extends Val
case object ValAtom extends Val
case class ValQuote(sym: String) extends Val
case class TheNeu(t: Val, neu: Neu) extends Val
case class ValEqv(t: Val, from: Val, to: Val) extends Val
case object ValSame extends Val
case class ValSucc (prev: Val) extends Val
case object ValNat extends Val
case object ValZero extends Val
case class ValFn(clo: Clo) extends Val
case class ValPi(arg_t: Val, ret_t: Clo) extends Val
case class ValCons(car: Val, cdr: Val) extends Val
case class ValSigma(arg_t: Val, cdr_t: Clo) extends Val
case object ValSole extends Val
case object ValTrivial extends Val
case object ValUniverse extends Val

trait Neu
case class NeuVar(name: String) extends Neu
case class NeuReplace(target: Neu, motive: TheVal, base: TheVal) extends Neu
case class NeuNatInd(target: Neu, motive: TheVal, base: TheVal, step: TheVal) extends Neu
case class NeuAbsurdInd(target: Neu, motive: TheVal) extends Neu
case class NeuAp(fn: Neu, arg: TheVal) extends Neu
case class NeuCar(pair: Neu) extends Neu
case class NeuCdr(pair: Neu) extends Neu

case class TheVal(t: Val, value: Val)

sealed trait Clo {
  def name: String
  def apply(value: Val): Either[Err, Val]
}

final case class NativeClo(name: String, fn: Val => Either[Err, Val]) extends Clo {
  def apply(value: Val): Either[Err, Val] =
    fn(value)
}

final case class EnvClo(env: Env, name: String, body: Exp) extends Clo {
  def apply(value: Val): Either[Err, Val] =
    eval(body, env.ext (name, value))
}
