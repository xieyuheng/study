package xieyuheng.tartlet

import java.util.UUID

trait Exp {
  def infer(ctx: Ctx): Either[Err, The]
}

trait Constructor extends Exp {
  def infer(ctx: Ctx): Either[Err, The] = {
    Left(Err(s"infer is not implemented for exp: ${this}"))
  }
}

trait Eliminator extends Exp

case class Var (name: String) extends Eliminator {

  /*
   ctx.lookup_type(x) == T
   --------------------------
   ctx :- Var(x) => T
   */
  def infer(ctx: Ctx): Either[Err, The] = {
    ctx.lookup_type(name) match {
      case Some(typeVal) => {
        for {
          typeExp <- typeVal.readback_val(ctx, ValUniverse)
        } yield The(typeExp, this)
      }
      case None =>
        Left(Err(s"can not find var: ${this} in ctx"))
    }
  }
}

case object Atom extends Type {


  /*
   -----------------
   ctx :- Atom => Universe
   */
  def infer(ctx: Ctx): Either[Err, The] =
    Right(The(Universe, Atom))
}

case class Quote (sym: String) extends Constructor

case class Eqv(
  t: Exp,
  from: Exp,
  to: Exp,
) extends Type {

  /*
    ctx :- T <= Universe
    ctx :- from <= T
    ctx :- to <= T
    --------------------
    ctx :- Eqv(T, from, to) => Universe
   */
  def infer(ctx: Ctx): Either[Err, The] = {
    for {
      t <- check(t, ctx, ValUniverse)
      typeVal <- eval(t, ctx.to_env)
      from <- check(from, ctx, typeVal)
      to <- check(to, ctx, typeVal)
    } yield The(Universe, Eqv(t, from, to))
  }
}

trait Type extends Exp

case class Replace (
  target: Exp,
  motive: Exp,
  base: Exp,
) extends Eliminator {

  /*
   ctx :- target => Eqv(T, from, to)
   ctx :- motive <= Pi(_: T, Universe)
   ctx :- base <= motive(from)
   --------------------
   ctx :- Replace(target, motive, base) => motive(to)
   */
  def infer(ctx: Ctx): Either[Err, The] = {
    for {
      the <- target.infer(ctx)
      res <- the.t match {
        case Eqv(t, from, to) =>
          for {
            typeVal <- eval(t, ctx.to_env)
            motive <- check(motive, ctx, ValPi(typeVal,
              NativeClo("_", _ => Right(ValUniverse))))
            motiveVal <- eval(motive, ctx.to_env)
            fromVal <- eval(from, ctx.to_env)
            baseType <- Ap.exe(motiveVal, fromVal)
            base <- check(base, ctx, baseType)
            toVal <- eval(to, ctx.to_env)
            typeVal <- Ap.exe(motiveVal, toVal)
            typeExp <- typeVal.readback_val(ctx, ValUniverse)
          } yield The(typeExp, Replace(the.value, motive, base))
        case _ =>
          Left(Err(
            s"expected the type to be Eqv(t, from, to), found: ${the.t}"))
      }
    } yield res
  }
}

object Replace {
  def exe(
    target: Val,
    motive: Val,
    base: Val,
  ): Either[Err, Val] = {
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

case object Same extends Constructor

case class Add1 (prev: Exp) extends Constructor

case class NatInd (
  target: Exp,
  motive: Exp,
  base: Exp,
  step: Exp,
) extends Eliminator {


  /*
   ctx :- target <= Nat
   ctx :- motive <= Pi(_: Nat, Universe)
   ctx :- base <= motive(Zero)
   ctx :- step <= Pi(
   --          prev: Nat, Pi(
   --            almost: motive(prev), motive(Add1(prev))))
   --------------------
   ctx :- NatInd(target, motive, base, step) => motive(target)
   */
  def infer(ctx: Ctx): Either[Err, The] = {
    for {
      target <- check(target, ctx, ValNat)
      motive <- check(motive, ctx, ValPi(ValNat,
        NativeClo("n", _ => Right(ValUniverse))))
      motiveVal <- eval(motive, ctx.to_env)
      targetVal <- eval(target, ctx.to_env)
      baseType <- Ap.exe(motiveVal, ValZero)
      base <- check(base, ctx, baseType)
      step <- check(step, ctx, NatInd.stepType(motiveVal))
      typeVal <- Ap.exe(motiveVal, targetVal)
      t <- typeVal.readback_val(ctx, ValUniverse)
    } yield The(t, NatInd(target, motive, base, step))
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
            Ap.exe(motive, ValAdd1(prev))))))
  }

  def exe(
    target: Val,
    motive: Val,
    base: Val,
    step: Val,
  ): Either[Err, Val] = {
    target match {
      case ValZero =>
        Right(base)
      case ValAdd1(prev) => {
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
            "ValAdd1(prev) | " +
            "TheNeu(ValNat, neutral): " +
            s"${target}"))
    }
  }
}

case object Nat extends Type {


  /*
   -----------------
   ctx :- Nat => Universe
   */
  def infer(ctx: Ctx): Either[Err, The] =
    Right(The(Universe, Nat))
}

case object Zero extends Constructor

case class Ap (
  rator: Exp,
  rand: Exp,
) extends Eliminator {

  /*
   ctx :- rator => Pi(x: A, R)
   ctx :- rand <= A
   -----------------
   ctx :- Ap(rator, rand) => R
   */
  def infer(ctx: Ctx): Either[Err, The] = {
    for {
      the <- rator.infer(ctx)
      t <- eval(the.t, ctx.to_env)
      res <- t match {
        case ValPi(arg_t, ret_t) => {
          for {
            rand <- check(rand, ctx, arg_t)
            argVal <- eval(rand, ctx.to_env)
            retVal <- ret_t.apply(argVal)
            retExp <- retVal.readback_val(ctx, ValUniverse)
          } yield The(retExp, Ap(the.value, rand))
        }
        case _ =>
          Left(Err("expected Pi, " + "found: ${t}"))
      }
    } yield res
  }
}

object Ap {
  def exe(
    fn: Val,
    arg: Val,
  ): Either[Err, Val] = {
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
  def apply(
    arg_t: Exp,
    ret_t: Exp,
  ): Type = Pi("_", arg_t, ret_t)
}

case class Fn (
  name: String,
  body: Exp,
) extends Constructor

case object Absurd extends Type {


  /*
   -----------------
   ctx :- Absurd => Universe
   */
  def infer(ctx: Ctx): Either[Err, The] =
    Right(The(Universe, Absurd))
}

case class AbsurdInd (
  target: Exp,
  motive: Exp,
) extends Eliminator {


  /*
   ctx :- target <= Absurd
   ctx :- motive <= Universe
   -----------------
   ctx :- AbsurdInd (target, motive) => Universe
   */
  def infer(ctx: Ctx): Either[Err, The] = {
    for {
      target <- check(target, ctx, ValAbsurd)
      motive <- check(motive, ctx, ValUniverse)
    } yield The(Universe, AbsurdInd(target, motive))
  }
}

object AbsurdInd {
  def exe(
    target: Val,
    motive: Val,
  ): Either[Err, Val] = {
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

case class Sigma (
  name: String,
  arg_t: Exp,
  cdr_t: Exp,
) extends Type {


  /*
   ctx :- A <= Universe
   ctx.ext(x, A) :- B <= Universe
   -----------------
   ctx :- Sigma(x: A, B) => Universe
   */
  def infer(ctx: Ctx): Either[Err, The] = {
    for {
      arg_t <- check(arg_t, ctx, ValUniverse)
      arg_tVal <- eval(arg_t, ctx.to_env)
      cdr_t <- check(cdr_t, ctx.ext(name, Bind(arg_tVal)), ValUniverse)
    } yield The(Universe, Pi(name, arg_t, cdr_t))
  }
}

case object Sole extends Constructor

case object Trivial extends Type {


  /*
   -----------------
   ctx :- Trivial => Universe
   */
  def infer(ctx: Ctx): Either[Err, The] =
    Right(The(Universe, Trivial))
}

case object Universe extends Type {


  /*
   -----------------
   ctx :- Universe => Universe
   */
  def infer(ctx: Ctx): Either[Err, The] =
    Right(The(Universe, Universe))
}

case class Pi (
  name: String,
  arg_t: Exp,
  ret_t: Exp,
) extends Type {

  /*
   ctx :- A <= Universe
   ctx.ext(x, A) :- B <= Universe
   -----------------
   ctx :- Pi(x: A, B) => Universe
   */
  def infer(ctx: Ctx): Either[Err, The] = {
    for {
      arg_t <- check(arg_t, ctx, ValUniverse)
      arg_tVal <- eval(arg_t, ctx.to_env)
      ret_t <- check(ret_t, ctx.ext(name, Bind(arg_tVal)), ValUniverse)
    } yield The(Universe, Pi(name, arg_t, ret_t))
  }
}

case class Car (
  pair: Exp,
) extends Eliminator {


  /*
   ctx: p => Sigma(x: A, D)
   ----------------
   ctx :- Car(p) => A
   */
  def infer(ctx: Ctx): Either[Err, The] = {
    for {
      the <- pair.infer(ctx)
      res <- the.t match {
        case Sigma(name, arg_t, cdr_t) =>
          for {
            arg_tVal <- eval(arg_t, ctx.to_env)
            arg_tExp <- arg_tVal.readback_val(ctx, ValUniverse)
          } yield The(arg_tExp, the.value)
        case _ =>
          Left(Err(
            s"expected the type to be Sigma(arg_t, cdr_t), found: ${the.t}"))
      }
    } yield res
  }
}

object Car {
  def exe(
    pair: Val,
  ): Either[Err, Val] = {
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

case class Cdr (
  pair: Exp,
) extends Eliminator {


  /*
   ctx: p => Sigma(x: A, D)
   ----------------
   ctx :- Cdr(p) => D subst (x, Car(p))
   */
  def infer(ctx: Ctx): Either[Err, The] = {
    for {
      the <- pair.infer(ctx)
      value <- eval(the.t, ctx.to_env)
      res <- value match {
        case ValSigma(arg_t, cdr_t) =>
          for {
            pairVal <- eval(the.value, ctx.to_env)
            carVal <- Car.exe(pairVal)
            realCdrType <- cdr_t.apply(carVal)
            cdr_tExp <- realCdrType.readback_val(ctx, ValUniverse)
          } yield The(cdr_tExp, the.value)
        case _ =>
          Left(Err(
            s"expected the type to be Sigma(arg_t, cdr_t), found: ${the.t}"))
      }
    } yield res
  }
}

object Cdr {
  def exe(
    pair: Val,
  ): Either[Err, Val] = {
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

case class Cons (car: Exp, cdr: Exp) extends Constructor

case class The (
  t: Exp,
  value: Exp,
) extends Exp {


  /*
   ctx :- T <= UNIVERSE
   ctx :- e <= T
   -----------------
   ctx :- e: T => T
   */
  def infer(ctx: Ctx): Either[Err, The] =
    for {
      t <- check(t, ctx, ValUniverse)
      tVal <- eval(t, ctx.to_env)
      value <- check(value, ctx, tVal)
    } yield The(t, value)

}
