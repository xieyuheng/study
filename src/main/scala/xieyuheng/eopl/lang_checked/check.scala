package xieyuheng.eopl.lang_checked

import pretty._

import xieyuheng.util.err._

object check {

  def infer(ctx: Ctx, exp: Exp): Either[Err, Type] = {
    exp match {

      case Var(name: String) =>
        ctx.lookup_type(name) match {
          case Some(t) =>
            Right(t)
          case None =>
            Left(Err(
              s"[infer fail]\n" ++
                s"undefined name: ${name}\n"
            ))
        }

      case Num(num: Int) =>
        Right(TypeInt())

      case Diff(exp1: Exp, exp2: Exp) =>
        val result = infer(ctx, exp1).flatMap {
          case TypeInt() =>
            infer(ctx, exp2).flatMap {
              case TypeInt() =>
                Right(TypeInt())
              case t2 =>
                Left(Err(
                  s"[infer fail]\n" ++
                    s"diff(x, y) type mismatch\n" ++
                    s"expecting y to be int\n" ++
                    s"y: ${pretty_exp(exp2)}\n" ++
                    s"type of y: ${pretty_type(t2)}\n"
                ))
            }
          case t1 =>
            Left(Err(
              s"[infer fail]\n" ++
                s"diff(x, y) type mismatch\n" ++
                s"expecting x to be int\n" ++
                s"x: ${pretty_exp(exp1)}\n" ++
                s"type of x: ${pretty_type(t1)}\n"
            ))
        }
        result_maybe_err(result, Err(
          s"[infer fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case ZeroP(exp1: Exp) =>
        val result = infer(ctx, exp1).flatMap {
          case TypeInt() =>
            Right(TypeBool())
          case t1 =>
            Left(Err(
              s"[infer fail]\n" ++
                s"zero_p(x) type mismatch\n" ++
                s"expecting x to be int\n" ++
                s"x: ${pretty_exp(exp1)}\n" ++
                s"type of x: ${pretty_type(t1)}\n"
            ))
        }
        result_maybe_err(result, Err(
          s"[infer fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case If(exp1: Exp, exp2: Exp, exp3: Exp) =>
        val result = infer(ctx, exp1).flatMap {
          case TypeBool() =>
            infer(ctx, exp2).flatMap { case t2 =>
              infer(ctx, exp3).flatMap { case t3 =>
                if (t2 == t3) {
                  Right(t2)
                } else {
                  Left(Err(
                    s"[infer fail]\n" ++
                      s"if (p) { x } else { y } type mismatch\n" ++
                      s"x: ${pretty_exp(exp2)}\n" ++
                      s"type of x: ${pretty_type(t2)}\n" ++
                      s"y: ${pretty_exp(exp3)}\n" ++
                      s"type of y: ${pretty_type(t3)}\n"
                  ))
                }
              }
            }
          case t1 =>
            Left(Err(
              s"[infer fail]\n" ++
                s"if (p) { x } else { y } type mismatch\n" ++
                s"expecting p to be bool\n" ++
                s"p: ${pretty_exp(exp1)}\n" ++
                s"type of p: ${pretty_type(t1)}\n"
            ))
        }
        result_maybe_err(result, Err(
          s"[infer fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case Let(name: String, exp1: Exp, body: Exp) =>
        val result = infer(ctx, exp1).flatMap { case t1 =>
          infer(ctx.ext(name, t1), body)
        }
        result_maybe_err(result, Err(
          s"[infer fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case Fn(name: String, arg_t: Type, body: Exp) =>
        val result = infer(ctx.ext(name, arg_t), body).flatMap { case t =>
          Right(TypeArrow(arg_t, t))
        }
        result_maybe_err(result, Err(
          s"[infer fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case Ap(target: Exp, arg: Exp) =>
        val result = infer(ctx, target).flatMap {
          case TypeArrow(arg_t, ret_t) =>
            infer(ctx, arg).flatMap { case t2 =>
              if (t2 == arg_t) {
                Right(ret_t)
              } else {
                Left(Err(
                  s"[infer fail]\n" ++
                    s"f(x) type mismatch\n" ++
                    s"expecting x to be of type: ${pretty_type(arg_t)}\n" ++
                    s"x: ${pretty_exp(arg)}\n" ++
                    s"type of x: ${pretty_type(t2)}\n"
                ))
              }
            }
          case t1 =>
            Left(Err(
              s"[infer fail]\n" ++
                s"f(x) type mismatch\n" ++
                s"expecting f to be function\n" ++
                s"f: ${pretty_exp(target)}\n" ++
                s"type of f: ${pretty_type(t1)}\n"
            ))
        }
        result_maybe_err(result, Err(
          s"[infer fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case LetRec(fn_name, arg_name, arg_t, ret_t, fn_body, body) =>
        val ctx2 = ctx.ext(fn_name, TypeArrow(arg_t, ret_t))
        val ctx3 = ctx2.ext(arg_name, arg_t)
        val result = infer(ctx3, fn_body).flatMap {
          case t1 =>
            if (t1 == ret_t) {
              infer(ctx2, body)
            } else {
              Left(Err(
                s"[infer fail]\n" ++
                  s"let rec return type mismatch\n" ++
                  s"expecting return type: ${pretty_type(ret_t)}\n" ++
                  s"infered return type: ${pretty_type(t1)}\n"
              ))
            }
        }
        result_maybe_err(result, Err(
          s"[infer fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case LetRecMutual(map: Map[String, (String, Type, Type, Exp)], body: Exp) =>
        val ctx2 = ctx.ext_map(map.map {
          case (fn_name, (arg_name, arg_t, ret_t, fn_body)) =>
            (fn_name, TypeArrow(arg_t, ret_t))
        })
        // BUG not all arg_name are in scope of all fn_body
        // TODO
        val ctx3 = ctx2.ext_map(map.map {
          case (fn_name, (arg_name, arg_t, ret_t, fn_body)) =>
            (arg_name, arg_t)
        })
        val annotated_return_type_map = map.map {
          case (fn_name, (arg_name, arg_t, ret_t, fn_body)) =>
            (fn_name, ret_t)
        }
        val result = check_map(ctx3, map).flatMap {
          case ok =>
            infer(ctx2, body)
        }
        result_maybe_err(result, Err(
          s"[infer fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case Sole() =>
        Right(TypeSole())

      case Do(exp1: Exp, body: Exp) =>
        infer(ctx, body)

      case AssertEq(exp1: Exp, exp2: Exp) =>
        val result = infer(ctx, exp1).flatMap { case t1 =>
          infer(ctx, exp2).flatMap { case t2 =>
            if (t1 == t2) {
              Right(t1)
            } else {
              Left(Err(
                s"[infer fail]\n" ++
                  s"assert_eq(x, y) type mismatch\n" ++
                  s"x: ${pretty_exp(exp1)}\n" ++
                  s"type of x: ${pretty_type(t1)}\n" ++
                  s"y: ${pretty_exp(exp2)}\n" ++
                  s"type of y: ${pretty_type(t2)}\n"
              ))
            }
          }
        }
        result_maybe_err(result, Err(
          s"[infer fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case Show(exp1: Exp) =>
        Right(TypeSole())

    }
  }

  def check(ctx: Ctx, exp: Exp, annotated: Type): Either[Err, Unit] = {
    infer(ctx, exp).flatMap { case infered =>
      if (annotated == infered) {
        Right(())
      } else {
        Left(Err(
          s"[check fail]\n" ++
            s"annotated type: ${pretty_type(annotated)}\n" ++
            s"infered type: ${pretty_type(infered)}\n"
        ))
      }
    }
  }

  def check_map(
    ctx: Ctx,
    map: Map[String, (String, Type, Type, Exp)],
  ): Either[Err, Unit] = {
    val init_result: Either[Err, Unit] = Right(())
    map.foldLeft(init_result) {
      case (result, (fn_name, (arg_name, arg_t, ret_t, body))) =>
        check(ctx, body, ret_t)
    }
  }

}
