package xieyuheng.tartlet

sealed trait Exp
final case class Var (name: String) extends Exp
final case object Atom extends Exp
final case class Quote(sym: String) extends Exp
final case class Eqv(t: Exp, from: Exp, to: Exp) extends Exp
final case class Replace(target: Exp, motive: Exp, base: Exp) extends Exp
final case object Same extends Exp
final case class Succ(prev: Exp) extends Exp
final case class NatInd(target: Exp, motive: Exp, base: Exp, step: Exp) extends Exp
final case object Nat extends Exp
final case object Zero extends Exp
final case class Ap(rator: Exp, rand: Exp) extends Exp
final case class Fn(name: String, body: Exp) extends Exp
final case object Absurd extends Exp
final case class AbsurdInd(target: Exp, motive: Exp) extends Exp
final case class Sigma(name: String, arg_t: Exp, cdr_t: Exp) extends Exp
final case object Sole extends Exp
final case object Trivial extends Exp
final case object Universe extends Exp
final case class Pi(name: String, arg_t: Exp, ret_t: Exp) extends Exp
final case class Car(pair: Exp) extends Exp
final case class Cdr(pair: Exp) extends Exp
final case class Cons(car: Exp, cdr: Exp) extends Exp
final case class The(t: Exp, value: Exp) extends Exp

object Replace {
  def exe(target: Val, motive: Val, base: Val): Either[Err, Val] = {
    target match {
      case ValSame =>
        Right(base)
      case TheNeu(ValEqv(t, from, to), neutral) => {
        for {
          typeVal <- Ap.exe(motive, to)
          baseType <- Ap.exe(motive, from)
        } yield TheNeu(typeVal,
          NeuReplace(
            neutral,
            TheVal(ValPi(t, NativeClo("x", _ => Right(ValUniverse))), motive),
            TheVal(baseType, base)))
      }
      case _ =>
        Left(Err(
          "target should be " +
            "ValSame | " +
            "TheNeu(ValEqv(t, from, to), neutral): " +
            s"${target}"))
    }
  }
}

object NatInd {
  def stepType(motive: Val): ValPi = {
    ValPi(ValNat,
      NativeClo("prev", prev =>
        for {
          almostType <- Ap.exe(motive, prev)
        } yield ValPi(almostType,
          NativeClo("almost", almost =>
            Ap.exe(motive, ValSucc(prev))))))
  }

  def exe(target: Val, motive: Val, base: Val, step: Val): Either[Err, Val] = {
    target match {
      case ValZero =>
        Right(base)
      case ValSucc(prev) => {
        for {
          f <- Ap.exe(step, prev)
          almost <- NatInd.exe(prev, motive, base, step)
          res <- Ap.exe(f, almost)
        } yield res
      }
      case TheNeu(ValNat, neutral) => {
        for {
          t <- Ap.exe(motive, target)
          baseType <- Ap.exe(motive, ValZero)
        } yield TheNeu(t,
          NeuNatInd(
            neutral,
            TheVal(ValPi(ValNat, NativeClo("k", k => Right(ValUniverse))), motive),
            TheVal(baseType, base),
            TheVal(NatInd.stepType(motive), step)))
      }
      case _ =>
        Left(Err(
          "target should be " +
            "ValZero | " +
            "ValSucc(prev) | " +
            "TheNeu(ValNat, neutral): " +
            s"${target}"))
    }
  }
}

object Ap {
  def exe(fn: Val, arg: Val): Either[Err, Val] = {
    fn match {
      case ValFn(clo) =>
        clo.apply(arg)
      case TheNeu(ValPi(arg_t, ret_t), neutral) =>
        for {
          t <- ret_t.apply(arg)
        } yield TheNeu(t, NeuAp(neutral, TheVal(arg_t, arg)))
      case _ =>
        Left(Err(
          "fn should be " +
            "ValFn(clo) | " +
            "TheNeu(ValPi(arg_t, ret_t), neutral): " +
            s"${fn}"))
    }
  }
}

object Arrow {
  def apply(arg_t: Exp, ret_t: Exp): Exp = Pi("_", arg_t, ret_t)
}

object AbsurdInd {
  def exe(target: Val, motive: Val): Either[Err, Val] = {
    target match {
      case TheNeu(ValAbsurd, neutral) =>
        Right(
          TheNeu(motive,
            NeuAbsurdInd(neutral, TheVal(ValUniverse, motive))))
      case _ =>
        Left(Err(
          s"target should be TheNeu(ValAbsurd, neutral): ${target}"))
    }
  }
}

object Car {
  def exe(pair: Val): Either[Err, Val] = {
    pair match {
      case ValCons(car, cdr) =>
        Right(car)
      case TheNeu(ValSigma(arg_t, cdr_t), neutral) => {
        Right(TheNeu(arg_t, NeuCar(neutral)))
      }
      case _ =>
        Left(Err(
          "pair should be " +
            "ValCons(car, cdr) | " +
            "TheNeu(ValSigma(arg_t, cdr_t), neutral): " +
            s"${pair}"))
    }
  }
}

object Cdr {
  def exe(pair: Val): Either[Err, Val] = {
    pair match {
      case ValCons(car, cdr) =>
        Right(cdr)
      case TheNeu(ValSigma(arg_t, cdr_t), neutral) => {
        for {
          carVal <- Car.exe(pair)
          realCdrType <- cdr_t.apply(carVal)
        } yield TheNeu(realCdrType, NeuCar(neutral))
      }
      case _ =>
        Left(Err(
          "pair should be " +
            "ValCons(car, cdr) | " +
            "TheNeu(ValSigma(arg_t, cdr_t), neutral): " +
            s"${pair}"))
    }
  }
}
