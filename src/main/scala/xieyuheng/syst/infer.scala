package xieyuheng.syst

import xieyuheng.util.err._

object infer {

  def apply(exp: Exp, ctx: Ctx): Either[Err, Type] =
    infer(exp: Exp, ctx: Ctx)

  def infer(exp: Exp, ctx: Ctx): Either[Err, Type] = {
    exp match {
      case Var(name: String) =>
        // ctx.lookup_type(x) == T
        // --------------------------
        // ctx :- Var(x) => T
        ctx.lookup_type(name) match {
          case Some(t) =>
            Right(t)
          case None =>
            Left(Err(s"can not find var: ${this} in ctx"))
        }
      case NatRec(t: Type, target: Exp, base: Exp, step: Exp) =>
        // ctx :- target <= Nat
        // ctx :- base <= T
        // ctx :- step <= Nat -> T -> T
        // -----------------------------------
        // ctx :- NatRec [T] (target, base, step) => T
        for {
          _ <- check(target, ctx, Nat())
          _ <- check(base, ctx, t)
          _ <- check(step, ctx, Arrow(Nat(), Arrow(t, t)))
        } yield t
      case The(t: Type, exp: Exp) =>
        // ctx :- e <= T
        // -----------------
        // ctx :- e: T => T
        Right(t)
      case Ap(rator: Exp, arg: Exp) =>
        // ctx :- rator => A -> R
        // ctx :- arg <= A
        // ---------------
        // ctx :- Ap (rator, arg) => R
        infer(rator, ctx) match {
          case Right(Arrow(arg_t, dep_t)) =>
            for {
              _ <- check(arg, ctx, arg_t)
            } yield dep_t
          case Left(err) =>
            Left(err)
          case _ =>
            Left(Err(s"the type of rator: ${rator} is not Arrow"))
        }
      case _ =>
        Left(Err(s"infer is not implemented for exp: ${this}"))
    }
  }

}
