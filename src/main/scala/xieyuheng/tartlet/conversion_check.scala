package xieyuheng.tartlet

import pretty._
import readback._

import xieyuheng.util.err._

object conversion_check {

  def apply(ctx: Ctx, t: Val, v1: Val, v2: Val): Either[Err, Unit] =
    conversion_check(ctx: Ctx, t: Val, v1: Val, v2: Val)

  def conversion_check(ctx: Ctx, t: Val, v1: Val, v2: Val): Either[Err, Unit] = {
    for {
      e1 <- readback_val(v1, t, ctx)
      e2 <- readback_val(v2, t, ctx)
      result <- {
        if (alpha_eq(e1, e2, Map(), Map())) {
          Right(())
        } else {
          Left(Err(
            s"conversion_check fail\n" ++
              s"e1 = ${pretty_exp(e1)}\n" ++
              s"e2 = ${pretty_exp(e2)}\n"))
        }
      }
    } yield result
  }
}
