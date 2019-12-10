package xieyuheng.cicada

import collection.immutable.ListMap

import eval._
import infer._
import subtype._

object check {

  def check(ctx: Ctx, v: Val, t: Val): Either[Err, Unit] = {
    for {
      s <- infer(ctx, v)
      result <- more_than(ctx, s, t)
    } yield result
  }

}
