package xieyuheng.systemt

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
      case RecNat(t: Type, target: Exp, base: Exp, step: Exp) =>
        // ctx :- target <= Nat
        // ctx :- base <= T
        // ctx :- step <= Nat -> T -> T
        // -----------------------------------
        // ctx :- RecNat [T] (target, base, step) => T
        for {
          _ok <- check(target, ctx, Nat)
          _ok <- check(base, ctx, t)
          _ok <- check(step, ctx, Arrow(Nat, Arrow(t, t)))
        } yield t
      case The(t: Type, exp: Exp) =>
        // ctx :- e <= T
        // -----------------
        // ctx :- e: T => T
        Right(t)
      case Ap(rator: Exp, rand: Exp) =>
        // ctx :- rator => A -> R
        // ctx :- rand <= A
        // ---------------
        // ctx :- Ap (rator, rand) => R
        infer(rator, ctx) match {
          case Right(Arrow(argType, retType)) =>
            for {
              _ok <- check(rand, ctx, argType)
            } yield retType
          case Left(errorMsg) =>
            Left(errorMsg)
          case _ =>
            Left(Err(s"the type of rator: ${rator} is not Arrow"))
        }
      case _ =>
        Left(Err(s"infer is not implemented for exp: ${this}"))
    }
  }

}
