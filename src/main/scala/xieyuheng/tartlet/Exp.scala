package xieyuheng.tartlet

sealed trait Exp
final case class Var(name: String) extends Exp
final case class Atom() extends Exp
final case class Quote(sym: String) extends Exp
final case class Eqv(t: Exp, from: Exp, to: Exp) extends Exp
final case class Same() extends Exp
final case class Replace(target: Exp, motive: Exp, base: Exp) extends Exp
final case class Nat() extends Exp
final case class Zero() extends Exp
final case class Succ(prev: Exp) extends Exp
final case class NatInd(target: Exp, motive: Exp, base: Exp, step: Exp) extends Exp
final case class Pi(name: String, arg_t: Exp, dep_t: Exp) extends Exp
final case class Fn(name: String, body: Exp) extends Exp
final case class Ap(rator: Exp, arg: Exp) extends Exp
final case class Absurd() extends Exp
final case class AbsurdInd(target: Exp, motive: Exp) extends Exp
final case class Sigma(name: String, arg_t: Exp, dep_t: Exp) extends Exp
final case class Cons(car: Exp, cdr: Exp) extends Exp
final case class Car(pair: Exp) extends Exp
final case class Cdr(pair: Exp) extends Exp
final case class Sole() extends Exp
final case class Trivial() extends Exp
final case class Universe() extends Exp
final case class The(t: Exp, value: Exp) extends Exp

object Replace {
  def exe(target: Val, motive: Val, base: Val): Either[Err, Val] = {
    target match {
      case ValSame() =>
        Right(base)
      case TheNeu(ValEqv(t, from, to), neu) => {
        for {
          t_val <- Ap.exe(motive, to)
          base_t <- Ap.exe(motive, from)
        } yield TheNeu(t_val,
          NeuReplace(
            neu,
            TheVal(ValPi(t, CloNative("x", _ => Right(ValUniverse()))), motive),
            TheVal(base_t, base)))
      }
      case _ =>
        Left(Err(
          "target should be " +
            "ValSame() | " +
            "TheNeu(ValEqv(t, from, to), neu): " +
            s"${target}"))
    }
  }
}

object NatInd {
  def stepType(motive: Val): ValPi = {
    ValPi(ValNat(),
      CloNative("prev", prev =>
        for {
          almostType <- Ap.exe(motive, prev)
        } yield ValPi(almostType,
          CloNative("almost", almost =>
            Ap.exe(motive, ValSucc(prev))))))
  }

  def exe(target: Val, motive: Val, base: Val, step: Val): Either[Err, Val] = {
    target match {
      case ValZero() =>
        Right(base)
      case ValSucc(prev) => {
        for {
          f <- Ap.exe(step, prev)
          almost <- NatInd.exe(prev, motive, base, step)
          res <- Ap.exe(f, almost)
        } yield res
      }
      case TheNeu(ValNat(), neu) => {
        for {
          t <- Ap.exe(motive, target)
          base_t <- Ap.exe(motive, ValZero())
        } yield TheNeu(t,
          NeuNatInd(
            neu,
            TheVal(ValPi(ValNat(), CloNative("k", k => Right(ValUniverse()))), motive),
            TheVal(base_t, base),
            TheVal(NatInd.stepType(motive), step)))
      }
      case _ =>
        Left(Err(
          "target should be " +
            "ValZero() | " +
            "ValSucc(prev) | " +
            "TheNeu(ValNat(), neu): " +
            s"${target}"))
    }
  }
}

object Ap {
  def exe(fn: Val, arg: Val): Either[Err, Val] = {
    fn match {
      case ValFn(clo) =>
        clo.ap(arg)
      case TheNeu(ValPi(arg_t, dep_t), neu) =>
        for {
          t <- dep_t.ap(arg)
        } yield TheNeu(t, NeuAp(neu, TheVal(arg_t, arg)))
      case _ =>
        Left(Err(
          "fn should be " +
            "ValFn(clo) | " +
            "TheNeu(ValPi(arg_t, dep_t), neu): " +
            s"${fn}"))
    }
  }
}

object Arrow {
  def apply(arg_t: Exp, dep_t: Exp): Exp = Pi("_", arg_t, dep_t)
}

object AbsurdInd {
  def exe(target: Val, motive: Val): Either[Err, Val] = {
    target match {
      case TheNeu(ValAbsurd(), neu) =>
        Right(
          TheNeu(motive,
            NeuAbsurdInd(neu, TheVal(ValUniverse(), motive))))
      case _ =>
        Left(Err(
          s"target should be TheNeu(ValAbsurd(), neu): ${target}"))
    }
  }
}

object Car {
  def exe(pair: Val): Either[Err, Val] = {
    pair match {
      case ValCons(car, cdr) =>
        Right(car)
      case TheNeu(ValSigma(arg_t, dep_t), neu) => {
        Right(TheNeu(arg_t, NeuCar(neu)))
      }
      case _ =>
        Left(Err(
          "pair should be " +
            "ValCons(car, cdr) | " +
            "TheNeu(ValSigma(arg_t, dep_t), neu): " +
            s"${pair}"))
    }
  }
}

object Cdr {
  def exe(pair: Val): Either[Err, Val] = {
    pair match {
      case ValCons(car, cdr) =>
        Right(cdr)
      case TheNeu(ValSigma(arg_t, dep_t), neu) => {
        for {
          car_val <- Car.exe(pair)
          real_dep_t <- dep_t.ap(car_val)
        } yield TheNeu(real_dep_t, NeuCar(neu))
      }
      case _ =>
        Left(Err(
          "pair should be " +
            "ValCons(car, cdr) | " +
            "TheNeu(ValSigma(arg_t, dep_t), neu): " +
            s"${pair}"))
    }
  }
}
