package xieyuheng.cicada

import collection.immutable.ListMap

import eval._
import infer._
import subtype._

object check {

  def check(env: Env, ctx: Ctx, exp: Exp, t: Val): Either[Err, Unit] = {
    exp match {
      case Obj(value_map: ListMap[String, Exp]) =>
        t match {
          case cl: ValCl =>
            var local_env = cl.env
            var local_ctx = ctx
            util.list_map_foreach_maybe_err(cl.type_map) {
              case (name, t_exp) =>
                for {
                  t_value <- eval(local_env, t_exp)
                  v_exp <- {
                    value_map.get(name) match {
                      case Some(v_exp) =>
                        Right(v_exp)
                      case None =>
                        Left(Err(
                          s"check fail, can not find a field of object in class, field: ${name}"
                        ))
                    }
                  }
                  result <- check(local_env, local_ctx, v_exp, t_value)
                  v_value <- eval(local_env, v_exp)
                  _ = {
                    local_env = local_env.ext(name, v_value)
                    local_ctx = local_ctx.ext(name, t_value)
                  }
                } yield result
            }

          case _ =>
            Left(Err(s"expecting class type but found: ${t}"))
        }

      case _ =>
        for {
          s <- infer(env, ctx, exp)
          result <- more_than(ctx, s, t)
        } yield result
    }
  }

}
