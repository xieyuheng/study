package xieyuheng.cicada

import collection.immutable.ListMap

import eval._
import infer._
import subtype._

object check {

  def check(ctx: Ctx, exp: Exp, t: Val): Either[Err, Unit] = {
    for {
      s <- infer(ctx, exp)
      result <- more_than(ctx, s, t)
    } yield result
  }

}
