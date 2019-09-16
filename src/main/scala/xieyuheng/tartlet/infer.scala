package xieyuheng.tartlet

object infer {

  def apply(exp: Exp, ctx: Ctx): Either[Err, The] =
    infer(exp: Exp, ctx: Ctx)

  def infer(exp: Exp, ctx: Ctx): Either[Err, The] = {
    exp match {
      case Var(name: String) =>
        // ctx.lookup_type(x) == T
        // --------------------------
        // ctx :- Var(x) => T
        ctx.lookup_type(name) match {
          case Some(typeVal) => {
            for {
              typeExp <- typeVal.readback_val(ctx, ValUniverse)
            } yield The(typeExp, exp)
          }
          case None =>
            Left(Err(s"can not find var: ${exp} in ctx"))
        }
      case Atom =>
        // -----------------
        // ctx :- Atom => Universe
        Right(The(Universe, Atom))
      case Eqv(t: Exp, from: Exp, to: Exp) =>
        // ctx :- T <= Universe
        // ctx :- from <= T
        // ctx :- to <= T
        // --------------------
        // ctx :- Eqv(T, from, to) => Universe
        for {
          t <- check(t, ctx, ValUniverse)
          typeVal <- eval(t, ctx.to_env)
          from <- check(from, ctx, typeVal)
          to <- check(to, ctx, typeVal)
        } yield The(Universe, Eqv(t, from, to))
      case Replace(target: Exp, motive: Exp, base: Exp) =>
        // ctx :- target => Eqv(T, from, to)
        // ctx :- motive <= Pi(_: T, Universe)
        // ctx :- base <= motive(from)
        // --------------------
        // ctx :- Replace(target, motive, base) => motive(to)
        for {
          the <- infer(target, ctx)
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
      case NatInd(target: Exp, motive: Exp, base: Exp, step: Exp) =>
        // ctx :- target <= Nat
        // ctx :- motive <= Pi(_: Nat, Universe)
        // ctx :- base <= motive(Zero)
        // ctx :- step <= Pi(
        // --          prev: Nat, Pi(
        // --            almost: motive(prev), motive(Add1(prev))))
        // --------------------
        // ctx :- NatInd(target, motive, base, step) => motive(target)
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
      case Nat =>
        // -----------------
        // ctx :- Nat => Universe
        Right(The(Universe, Nat))
      case Ap(rator: Exp, rand: Exp) =>
        // ctx :- rator => Pi(x: A, R)
        // ctx :- rand <= A
        // -----------------
        // ctx :- Ap(rator, rand) => R
        for {
          the <- infer(rator, ctx)
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
      case Absurd =>
        // -----------------
        // ctx :- Absurd => Universe
        Right(The(Universe, Absurd))
      case AbsurdInd(target: Exp, motive: Exp) =>
        // ctx :- target <= Absurd
        // ctx :- motive <= Universe
        // -----------------
        // ctx :- AbsurdInd (target, motive) => Universe
        for {
          target <- check(target, ctx, ValAbsurd)
          motive <- check(motive, ctx, ValUniverse)
        } yield The(Universe, AbsurdInd(target, motive))
      case Sigma(name: String, arg_t: Exp, cdr_t: Exp) =>
        // ctx :- A <= Universe
        // ctx.ext(x, A) :- B <= Universe
        // -----------------
        // ctx :- Sigma(x: A, B) => Universe
        for {
          arg_t <- check(arg_t, ctx, ValUniverse)
          arg_tVal <- eval(arg_t, ctx.to_env)
          cdr_t <- check(cdr_t, ctx.ext(name, Bind(arg_tVal)), ValUniverse)
        } yield The(Universe, Pi(name, arg_t, cdr_t))
      case Trivial =>
        // -----------------
        // ctx :- Trivial => Universe
        Right(The(Universe, Trivial))
      case Universe =>
        // -----------------
        // ctx :- Universe => Universe
        Right(The(Universe, Universe))
      case Pi(name: String, arg_t: Exp, ret_t: Exp) =>
        // ctx :- A <= Universe
        // ctx.ext(x, A) :- B <= Universe
        // -----------------
        // ctx :- Pi(x: A, B) => Universe
        for {
          arg_t <- check(arg_t, ctx, ValUniverse)
          arg_tVal <- eval(arg_t, ctx.to_env)
          ret_t <- check(ret_t, ctx.ext(name, Bind(arg_tVal)), ValUniverse)
        } yield The(Universe, Pi(name, arg_t, ret_t))
      case Car(pair: Exp) =>
        // ctx: p => Sigma(x: A, D)
        // ----------------
        // ctx :- Car(p) => A
        for {
          the <- infer(pair, ctx)
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
      case Cdr(pair: Exp) =>
        // ctx: p => Sigma(x: A, D)
        // ----------------
        // ctx :- Cdr(p) => D subst (x, Car(p))
        for {
          the <- infer(pair, ctx)
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
      case The(t: Exp, value: Exp) =>
        // ctx :- T <= UNIVERSE
        // ctx :- e <= T
        // -----------------
        // ctx :- e: T => T
        for {
          t <- check(t, ctx, ValUniverse)
          tVal <- eval(t, ctx.to_env)
          value <- check(value, ctx, tVal)
        } yield The(t, value)
      case _ =>
        Left(Err(s"infer is not implemented for exp: ${exp}"))
    }
  }
}
