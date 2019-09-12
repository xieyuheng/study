package xieyuheng.minitt

import xieyuheng.minitt.pretty._
import xieyuheng.minitt.readback._

object check {

  def check_decl(i: Int, env: Env, ctx: Ctx, decl: Decl): Either[Err, Ctx] = {
    decl match {
      case Let(pat, t_exp, e) =>
        for {
          _ <- check_type(i, env, ctx, t_exp)
          t = eval(t_exp, env)
          ctx1 <- ctx.ext(pat, t, eval(e, env))
        } yield ctx1
      case decl @ Letrec(pat, t_exp, e) =>
        for {
          _ <- check_type(i, env, ctx, t_exp)
          t = eval(t_exp, env)
          fresh = ValNeu(NeuVar(fresh_name(i)))
          ctx1 <- ctx.ext(pat, t, fresh)
          _ <- check(i + 1, EnvPat(pat, fresh, env), ctx1, e, t)
          v = eval(e, EnvDecl(decl, env))
          ctx2 <- ctx.ext(pat, t, v)
        } yield ctx2
    }
  }

  def check_type(i: Int, env: Env, ctx: Ctx, t: Exp): Either[Err, Unit] = {
    t match {
      case Pi(pat: Pat, arg_t: Exp, dep_t: Exp) =>
        for {
          _ <- check_type(i, env, ctx, arg_t)
          fresh = ValNeu(NeuVar(fresh_name(i)))
          ctx1 <- ctx.ext(pat, eval(arg_t, env), fresh)
          _ <- check_type(i + 1, EnvPat(pat, fresh, env), ctx1, dep_t)
        } yield ()
      case Sigma(pat: Pat, arg_t: Exp, dep_t: Exp) =>
        for {
          _ <- check_type(i, env, ctx, arg_t)
          fresh = ValNeu(NeuVar(fresh_name(i)))
          ctx1 <- ctx.ext(pat, eval(arg_t, env), fresh)
          _ <- check_type(i + 1, EnvPat(pat, fresh, env), ctx1, dep_t)
        } yield ()
      case Univ() => Right(())
      case _ => check(i, env, ctx, t, ValUniv())
    }
  }

  def check(i: Int, env: Env, ctx: Ctx, e: Exp, t: Val): Either[Err, Unit] = {
    (e, t) match {
      case (Fn(pat: Pat, body: Exp), ValPi(arg_t: Val, clo)) =>
        val fresh = ValNeu(NeuVar(fresh_name(i)))
        for {
          ctx1 <- ctx.ext(pat, arg_t, fresh)
          _ <- check(i + 1, EnvPat(pat, fresh, env), ctx1, body, clo.ap(fresh))
        } yield ()
      case (Cons(car: Exp, cdr: Exp), ValSigma(arg_t: Val, clo)) =>
        for {
          _ <- check(i, env, ctx, car, arg_t)
          _ <- check(i, env, ctx, cdr, clo.ap(eval(car, env)))
        } yield ()
      case (Pi(pat: Pat, arg_t: Exp, dep_t: Exp), ValUniv()) =>
        for {
          _ <- check(i, env, ctx, arg_t, ValUniv())
          fresh = ValNeu(NeuVar(fresh_name(i)))
          ctx1 <- ctx.ext(pat, eval(arg_t, env), fresh)
          _ <- check(i + 1, EnvPat(pat, fresh, env), ctx1, dep_t, ValUniv())
        } yield ()
      case (Sigma(pat: Pat, arg_t: Exp, dep_t: Exp), ValUniv()) =>
        for {
          _ <- check(i, env, ctx, arg_t, ValUniv())
          fresh = ValNeu(NeuVar(fresh_name(i)))
          ctx1 <- ctx.ext(pat, eval(arg_t, env), fresh)
          _ <- check(i + 1, EnvPat(pat, fresh, env), ctx1, dep_t, ValUniv())
        } yield ()
      case (Data(tag, body), ValSum(CloMat(mats, env2))) =>
        mats.get(tag) match {
          case Some(t) => check(i, env, ctx, body, eval(t, env2))
          case None =>
            Left(Err(
              s"fail to find tag: ${tag}"))
        }
      case (Mat(mats: Map[String, Exp]), ValPi(ValSum(clo_mat), clo)) =>
        val mat_tags = mats.keys.toSet
        val sum_tags = clo_mat.mats.keys.toSet
        if (mat_tags != sum_tags) {
          Left(Err(
            s"sum type miss match\n" ++
              s"mat_tags: ${mat_tags}\n" ++
              s"sum_tags: ${sum_tags}\n"))
        } else {
          val results = mats.map { case (tag, e) =>
            val arg_t = eval(clo_mat.mats.get(tag).get, clo_mat.env)
            check(i, env, ctx, e, ValPi(arg_t, CloTag(tag, clo)))
          }
          results.find(_.isLeft) match {
            case Some(left) => left
            case None => Right(())
          }
        }
      case (Sum(mats: Map[String, Exp]), ValUniv()) =>
        val results = mats.values.map { case e => check(i, env, ctx, e, ValUniv()) }
        results.find(_.isLeft) match {
          case Some(left) => left
          case None => Right(())
        }
      case (Sole(), ValTrivial()) => Right(())
      case (Trivial(), ValUniv()) => Right(())
      case (e, t) =>
        for (u <- check_infer(i, env, ctx, e)) yield {
          if (readback_val(i, t) == readback_val(i, u)) {
            Right(())
          } else {
            Left(Err(
              s"check fail\n" ++
                s"expect type: ${prettyVal(t)}\n" ++
                s"exp: ${prettyExp(e)}\n" ++
                s"actual type: ${prettyVal(u)}\n"))
          }
        }
    }
  }

  def check_infer(i: Int, env: Env, ctx: Ctx, e: Exp): Either[Err, Val] = {
    e match {
      case Var(name: String) =>
        ctx.lookup(name) match {
          case Some(t) => Right(t)
          case None => Left(Err(s"can not find var: ${name} in ctx"))
        }
      case Ap(fn: Exp, arg: Exp) =>
        for {
          t <- check_infer(i, env, ctx, fn)
          t <- {
            t match {
              case ValPi(arg_t: Val, clo: Clo) => Right(t)
              case _ =>
                Left(Err(
                  s"expect pi\n" ++
                    s"e: ${prettyExp(e)}\n" ++
                    s"actual: ${prettyVal(t)}\n"))
            }
          }
          ValPi(arg_t: Val, clo: Clo) = t
          _ <- check(i, env, ctx, arg, arg_t)
        } yield clo.ap(eval(arg, env))
      case Car(pair: Exp) =>
        for {
          t <- check_infer(i, env, ctx, pair)
          t <- {
            t match {
              case ValSigma(arg_t: Val, clo: Clo) => Right(t)
              case _ =>
                Left(Err(
                  s"expect sigma\n" ++
                    s"e: ${prettyExp(e)}\n" ++
                    s"actual: ${prettyVal(t)}\n"))
            }
          }
          ValSigma(arg_t: Val, clo: Clo) = t
        } yield arg_t
      case Cdr(pair: Exp) =>
        for {
          t <- check_infer(i, env, ctx, pair)
          t <- {
            t match {
              case ValSigma(arg_t: Val, clo: Clo) => Right(t)
              case _ =>
                Left(Err(
                  s"expect sigma\n" ++
                    s"e: ${prettyExp(e)}\n" ++
                    s"actual: ${prettyVal(t)}\n"))
            }
          }
          ValSigma(arg_t: Val, clo: Clo) = t
        } yield clo.ap(eval.car(eval(pair, env)))
      case _ => Left(Err(s"can not check_infer: ${prettyExp(e)}"))
    }
  }

}
