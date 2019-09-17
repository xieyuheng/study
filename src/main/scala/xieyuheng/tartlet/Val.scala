package xieyuheng.tartlet

sealed trait Val
final case class TheNeu(t: Val, neu: Neu) extends Val
final case class ValAbsurd() extends Val
final case class ValAtom() extends Val
final case class ValQuote(sym: String) extends Val
final case class ValEqv(t: Val, from: Val, to: Val) extends Val
final case class ValSame() extends Val
final case class ValNat() extends Val
final case class ValZero() extends Val
final case class ValSucc (prev: Val) extends Val
final case class ValPi(arg_t: Val, ret_t: Clo) extends Val
final case class ValFn(clo: Clo) extends Val
final case class ValSigma(arg_t: Val, cdr_t: Clo) extends Val
final case class ValCons(car: Val, cdr: Val) extends Val
final case class ValSole() extends Val
final case class ValTrivial() extends Val
final case class ValUniverse() extends Val

sealed trait Neu
final case class NeuVar(name: String) extends Neu
final case class NeuReplace(target: Neu, motive: TheVal, base: TheVal) extends Neu
final case class NeuNatInd(target: Neu, motive: TheVal, base: TheVal, step: TheVal) extends Neu
final case class NeuAbsurdInd(target: Neu, motive: TheVal) extends Neu
final case class NeuAp(fn: Neu, arg: TheVal) extends Neu
final case class NeuCar(pair: Neu) extends Neu
final case class NeuCdr(pair: Neu) extends Neu

case class TheVal(t: Val, value: Val)

sealed trait Clo {
  def name: String
  def ap(value: Val): Either[Err, Val]
}

final case class CloNative(name: String, fn: Val => Either[Err, Val]) extends Clo {
  def ap(value: Val): Either[Err, Val] =
    fn(value)
}

final case class CloEnv(env: Env, name: String, body: Exp) extends Clo {
  def ap(value: Val): Either[Err, Val] =
    eval(body, env.ext (name, value))
}
