package xieyuheng.cicada

import collection.immutable.ListMap

import infer._
import subtype._

object check {

  def check(ctx: Ctx, exp: Exp, t: Val): Either[Err, Unit] = {
    for {
      s <- infer(ctx, exp)
      result <- subtype(ctx, s, t)
    } yield result
  }

}
