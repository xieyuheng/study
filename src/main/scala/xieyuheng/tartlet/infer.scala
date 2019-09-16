package xieyuheng.tartlet

import readback._

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
          case Some(t_val) => {
            for {
              t_exp <- readback_val(t_val, ValUniverse, ctx)
            } yield The(t_exp, exp)
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
          t_val <- eval(t, ctx.to_env)
          from <- check(from, ctx, t_val)
          to <- check(to, ctx, t_val)
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
                t_val <- eval(t, ctx.to_env)
                motive <- check(motive, ctx, ValPi(t_val,
                  CloNative("_", _ => Right(ValUniverse))))
                motive_val <- eval(motive, ctx.to_env)
                from_val <- eval(from, ctx.to_env)
                base_t <- Ap.exe(motive_val, from_val)
                base <- check(base, ctx, base_t)
                to_val <- eval(to, ctx.to_env)
                t_val <- Ap.exe(motive_val, to_val)
                t_exp <- readback_val(t_val, ValUniverse, ctx)
              } yield The(t_exp, Replace(the.value, motive, base))
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
        // --            almost: motive(prev), motive(Succ(prev))))
        // --------------------
        // ctx :- NatInd(target, motive, base, step) => motive(target)
        for {
          target <- check(target, ctx, ValNat)
          motive <- check(motive, ctx, ValPi(ValNat,
            CloNative("n", _ => Right(ValUniverse))))
          motive_val <- eval(motive, ctx.to_env)
          target_val <- eval(target, ctx.to_env)
          base_t <- Ap.exe(motive_val, ValZero)
          base <- check(base, ctx, base_t)
          step <- check(step, ctx, NatInd.stepType(motive_val))
          t_val <- Ap.exe(motive_val, target_val)
          t <- readback_val(t_val, ValUniverse, ctx)
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
                arg_t_val <- eval(rand, ctx.to_env)
                ret_t_val <- ret_t.ap(arg_t_val)
                ret_t_exp <- readback_val(ret_t_val, ValUniverse, ctx)
              } yield The(ret_t_exp, Ap(the.value, rand))
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
          arg_t_val <- eval(arg_t, ctx.to_env)
          cdr_t <- check(cdr_t, ctx.ext(name, Bind(arg_t_val)), ValUniverse)
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
          arg_t_val <- eval(arg_t, ctx.to_env)
          ret_t <- check(ret_t, ctx.ext(name, Bind(arg_t_val)), ValUniverse)
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
                arg_t_val <- eval(arg_t, ctx.to_env)
                arg_t_exp <- readback_val(arg_t_val, ValUniverse, ctx)
              } yield The(arg_t_exp, the.value)
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
                pair_val <- eval(the.value, ctx.to_env)
                car_val <- Car.exe(pair_val)
                real_cdr_t <- cdr_t.ap(car_val)
                cdr_t_exp <- readback_val(real_cdr_t, ValUniverse, ctx)
              } yield The(cdr_t_exp, the.value)
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
          t_val <- eval(t, ctx.to_env)
          value <- check(value, ctx, t_val)
        } yield The(t, value)
      case _ =>
        Left(Err(s"infer is not implemented for exp: ${exp}"))
    }
  }
}
