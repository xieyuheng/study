package xieyuheng.eopl.lang_nameless

import pretty._

import xieyuheng.eopl.util._

object eval {

  def eval_idx(idx: Idx, idx_env: IdxEnv): Either[Err, Val] = {
    idx match {
      case IdxVar(name: String, index: Int) =>
        idx_env.lookup_val(index) match {
          case Some(value) =>
            Right(value)
          case None =>
            Left(Err(
              s"[eval_idx fail]\n" ++
                s"undefined name: ${name}\n" ++
                s"index out of bound: ${index}\n"
            ))
        }
      case IdxNum(num: Int) =>
        Right(ValNum(num))
      case IdxDiff(idx1: Idx, idx2: Idx) =>
        val result = for {
          val1 <- eval_idx(idx1, idx_env)
          val2 <- eval_idx(idx2, idx_env)
          result <- {
            (val1, val2) match {
              case (ValNum(x), ValNum(y)) =>
                Right(ValNum(x - y))
              case (x, y) =>
                Left(Err(
                  s"[eval_idx fail]\n" ++
                    s"diff(x, y) type mismatch\n" ++
                    s"expecting number\n" ++
                    s"x: ${pretty_val(x)}\n" ++
                    s"y: ${pretty_val(y)}\n"
                ))
            }
          }
        } yield result
        result_maybe_err(result, Err(
          s"[eval_idx fail]\n" ++
            s"idx: ${pretty_idx(idx)}\n"
        ))
      case IdxZeroP(idx1: Idx) =>
        val result = for {
          val1 <- eval_idx(idx1, idx_env)
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
                  s"[eval_idx fail]\n" ++
                    s"zero_p(x, y) type mismatch\n" ++
                    s"expecting number\n" ++
                    s"x: ${pretty_val(x)}\n"
                ))
            }
          }
        } yield result
        result_maybe_err(result, Err(
          s"[eval_idx fail]\n" ++
            s"idx: ${pretty_idx(idx)}\n"
        ))
      case IdxIf(idx1: Idx, idx2: Idx, idx3: Idx) =>
        val result = for {
          val1 <- eval_idx(idx1, idx_env)
          result <- {
            val1 match {
              case ValBool(true) =>
                eval_idx(idx2, idx_env)
              case ValBool(false) =>
                eval_idx(idx3, idx_env)
              case x =>
                Left(Err(
                  s"[eval_idx fail]\n" ++
                    s"if x then _ else _ type mismatch\n" ++
                    s"expecting bool\n" ++
                    s"x: ${pretty_val(x)}\n"
                ))
            }
          }
        } yield result
        result_maybe_err(result, Err(
          s"[eval_idx fail]\n" ++
            s"idx: ${pretty_idx(idx)}\n"
        ))
      case IdxLet(name: String, idx1: Idx, body: Idx) =>
        val result = for {
          val1 <- eval_idx(idx1, idx_env)
          result <- eval_idx(body, idx_env.ext_let(val1))
        } yield result
        result_maybe_err(result, Err(
          s"[eval_idx fail]\n" ++
            s"idx: ${pretty_idx(idx)}\n"
        ))
      case IdxFn(name: String, body: Idx) =>
        Right(ValFn(name, body, idx_env))
      case IdxAp(target: Idx, arg: Idx) =>
        val result = for {
          f <- eval_idx(target, idx_env)
          v <- eval_idx(arg, idx_env)
          result <- {
            f match {
              case f: ValFn =>
                eval_idx(f.body, f.idx_env.ext_let(v))
              case _ =>
                Left(Err(
                  s"[eval_idx fail]\n" ++
                    s"f(x) type mismatch\n" ++
                    s"expecting function\n" ++
                    s"f: ${pretty_val(f)}\n"
                ))
            }
          }
        } yield result
        result_maybe_err(result, Err(
          s"[eval_idx fail]\n" ++
            s"idx: ${pretty_idx(idx)}\n"
        ))
    }
  }

}
