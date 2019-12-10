package xieyuheng.cicada

import collection.immutable.ListMap

import eval._
import infer._
import subtype._

object check {

  def check(env: Env, ctx: Ctx, exp: Exp, t: Val): Either[Err, Unit] = {
    for {
      s <- infer(env, ctx, exp)
      result <- more_than(ctx, s, t)
    } yield result
  }

}
