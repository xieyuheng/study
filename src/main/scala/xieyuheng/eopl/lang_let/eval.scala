package xieyuheng.eopl.lang_let

import xieyuheng.util.err._
import pretty._

object eval {

  def eval(exp: Exp, env: Env): Either[Err, Val] = {
    exp match {

      case Var(name: String) =>
        env.lookup_val(name) match {
          case Some(value) =>
            Right(value)
          case None =>
            Left(Err(
              s"[eval fail]\n" ++
                s"undefined name: ${name}\n"
            ))
        }

      case Num(num: Int) =>
        Right(ValNum(num))

      case Diff(exp1: Exp, exp2: Exp) =>
        val result = for {
          val1 <- eval(exp1, env)
          val2 <- eval(exp2, env)
          result <- {
            (val1, val2) match {
              case (ValNum(x), ValNum(y)) =>
                Right(ValNum(x - y))
              case (x, y) =>
                Left(Err(
                  s"[eval fail]\n" ++
                    s"diff(x, y) type mismatch\n" ++
                    s"expecting number\n" ++
                    s"x: ${pretty_val(x)}\n" ++
                    s"y: ${pretty_val(y)}\n"
                ))
            }
          }
        } yield result
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case ZeroP(exp1: Exp) =>
        val result = for {
          val1 <- eval(exp1, env)
          result <- {
            val1 match {
              case ValNum(x) =>
                if (x == 0) {
                  Right(ValBool(true))
                } else {
                  Right(ValBool(false))
                }
              case x =>
                Left(Err(
                  s"[eval fail]\n" ++
                    s"zero_p(x, y) type mismatch\n" ++
                    s"expecting number\n" ++
                    s"x: ${pretty_val(x)}\n"
                ))
            }
          }
        } yield result
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case If(exp1: Exp, exp2: Exp, exp3: Exp) =>
        val result = for {
          val1 <- eval(exp1, env)
          result <- {
            val1 match {
              case ValBool(true) =>
                eval(exp2, env)
              case ValBool(false) =>
                eval(exp3, env)
              case x =>
                Left(Err(
                  s"[eval fail]\n" ++
                    s"if x then _ else _ type mismatch\n" ++
                    s"expecting bool\n" ++
                    s"x: ${pretty_val(x)}\n"
                ))
            }
          }
        } yield result
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case Let(name: String, exp1: Exp, body: Exp) =>
        val result = for {
          val1 <- eval(exp1, env)
          result <- eval(body, env.ext_let(name, val1))
        } yield result
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case Sole() =>
        Right(ValSole())

      case Do(exp1: Exp, body: Exp) =>
        val result = for {
          _ <- eval(exp1, env)
          result <- eval(body, env)
        } yield result
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case AssertEq(exp1, exp2) =>
        val result = for {
          val1 <- eval(exp1, env)
          val2 <- eval(exp2, env)
          result <- {
            if (val1 == val2) {
              Right(ValSole())
            } else {
              Left(Err(
                s"[assert_eq fail]\n" ++
                  s">>> ${pretty_exp(exp1)}\n" ++
                  s"=== ${pretty_val(val1)}\n" ++
                  s">>> ${pretty_exp(exp2)}\n" ++
                  s"=== ${pretty_val(val2)}\n"
              ))
            }
          }
        } yield result
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case Show(exp1) =>
        val result = for {
          val1 <- eval(exp1, env)
          result <- {
            println(s">>> ${pretty_exp(exp1)}")
            println(s"=== ${pretty_val(val1)}")
            Right(ValSole())
          }
        } yield result
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

    }
  }

}
