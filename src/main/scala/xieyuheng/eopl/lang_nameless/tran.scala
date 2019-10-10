package xieyuheng.eopl.lang_nameless

import pretty._

import xieyuheng.util.err._

object tran {

  def tran_nameless(exp: Exp, idx_ctx: IdxCtx): Either[Err, Idx] = {
    exp match {
      case Var(name: String) =>
        idx_ctx.lookup_index(name) match {
          case Some(index) =>
            Right(IdxVar(name, index))
          case None =>
            Left(Err(
              s"[tran_nameless fail]\n" ++
                s"undefined name: ${name}\n"
            ))
        }
      case Num(num: Int) =>
        Right(IdxNum(num))
      case Diff(exp1: Exp, exp2: Exp) =>
        val result = for {
          idx_exp1 <- tran_nameless(exp1, idx_ctx)
          idx_exp2 <- tran_nameless(exp2, idx_ctx)
          result <- {
            Right(IdxDiff(idx_exp1, idx_exp2))
          }
        } yield result
        result_maybe_err(result, Err(
          s"[tran_nameless fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))
      case ZeroP(exp1: Exp) =>
        val result = for {
          idx_exp1 <- tran_nameless(exp1, idx_ctx)
          result <- {
            Right(IdxZeroP(idx_exp1))
          }
        } yield result
        result_maybe_err(result, Err(
          s"[tran_nameless fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))
      case If(exp1: Exp, exp2: Exp, exp3: Exp) =>
        val result = for {
          idx_exp1 <- tran_nameless(exp1, idx_ctx)
          idx_exp2 <- tran_nameless(exp2, idx_ctx)
          idx_exp3 <- tran_nameless(exp3, idx_ctx)
          result <- {
            Right(IdxIf(idx_exp1, idx_exp2, idx_exp3))
          }
        } yield result
        result_maybe_err(result, Err(
          s"[tran_nameless fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))
      case Let(name: String, exp1: Exp, body: Exp) =>
        val result = for {
          idx_exp1 <- tran_nameless(exp1, idx_ctx)
          idx_body <- tran_nameless(body, idx_ctx.ext_let(name))
          result <- {
            Right(IdxLet(name, idx_exp1, idx_body))
          }
        } yield result
        result_maybe_err(result, Err(
          s"[tran_nameless fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))
      case Fn(name: String, body: Exp) =>
        val result = for {
          idx_body <- tran_nameless(body, idx_ctx.ext_let(name))
          result <- {
            Right(IdxFn(name, idx_body))
          }
        } yield result
        result_maybe_err(result, Err(
          s"[tran_nameless fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))
      case Ap(target: Exp, arg: Exp) =>
        val result = for {
          idx_target <- tran_nameless(target, idx_ctx)
          idx_arg <- tran_nameless(arg, idx_ctx)
          result <- {
            Right(IdxAp(idx_target, idx_arg))
          }
        } yield result
        result_maybe_err(result, Err(
          s"[tran_nameless fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))
    }
  }

}
