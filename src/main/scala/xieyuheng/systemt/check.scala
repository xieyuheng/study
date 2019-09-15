package xieyuheng.systemt

object check {

  def apply(exp: Exp, ctx: Ctx, t: Type) =
    check(exp: Exp, ctx: Ctx, t: Type)

  def check(exp: Exp, ctx: Ctx, t: Type): Either[Err, Unit] = {
    exp match {
      case Succ(prev: Exp) =>
         // ctx :- prev <= Nat
         // -------------------
         // ctx :- Succ (prev) <= Nat
        t match {
          case Nat =>
            check(prev, ctx, t)
          case _ =>
            Left(Err("the type of Succ should be Nat"))
        }
      case Zero =>
         // -------------------
         // ctx :- Zero <= Nat
        t match {
          case Nat => Right(())
          case _ =>
            Left(Err("the type of Zero should be Nat"))
        }
      case Fn(name: String, body: Exp) =>
         // ctx.ext (x, A) :- e <= B
         // -------------------------
         // ctx :- Fn (x, e) <= A -> B
        t match {
          case Arrow(argType, retType) =>
            check(body, ctx.ext(name, argType), retType)
          case _ =>
            Left(Err(
              s"type of Fn is not Arrow, exp: ${this}"))
        }
      case _ =>
         // ctx :- e => T2
         // T2 == T
         // -----------------
         // ctx :- e <= T
        for (t2 <- infer(exp, ctx)) yield {
          if (t == t2) {
            ()
          } else {
            Err(s"check is not implemented for exp: ${this}")
          }
        }
    }
  }

}
