package xieyuheng.cicada

import infer._
import pretty._
import fulfill._
import readback._

object check {

  def apply(seed: Seed, env: Env, ctx: Ctx, exp: Exp, t: Val): Either[Err, Unit] =
    check(seed: Seed, env: Env, ctx: Ctx, exp: Exp, t: Val)

  def check(seed: Seed, env: Env, ctx: Ctx, exp: Exp, t: Val): Either[Err, Unit] = {
    exp match {
      case Var(name: String) =>
        ctx.lookup_type(name) match {
          case Some(u) =>
            fulfill_val(u, t)
          case None =>
            Left(Err(
              s"[check fail]\n" ++
                s"name: ${name}\n"))
        }
      case Type(level: Int) =>
        t match {
          case t: ValType =>
            if (level < t.level) {
              Right(())
            } else {
              Left(Err(
                s"[check fail]\n" ++
                  s"exp level: ${level}\n" ++
                  s"t level: ${t.level}\n"))
            }
          case _ =>
            Left(Err(
              s"[check fail]\n" ++
                s"exp: ${pretty_exp(exp)}\n" ++
                s"t: ${pretty_val(t)}\n"))
        }
      case Pi(arg_name: String, arg_t: Exp, dep_t: Exp) =>
        t match {
          case t: ValType =>
            for {
              _ <- check_level(seed, env, ctx, arg_t, t.level)
              _ <- check_level(seed, env, ctx, dep_t, t.level)
            } yield ()
          case _ =>
            Left(Err(
              s"[check fail]\n" ++
                s"exp: ${pretty_exp(exp)}\n" ++
                s"t: ${pretty_val(t)}\n"))
        }
      case Fn(arg_name: String, arg_t: Exp, dep_t: Exp, body: Exp) =>
        t match {
          case pi: ValPi =>
            val art_t_val = eval(arg_t, env)
            val fresh = gen_fresh(seed, pi.arg_t, Some(pi.arg_name))
            for {
              _ <- fulfill_val(pi.arg_t, art_t_val)
              _ <- check(
                seed_inc(seed),
                env,
                ctx.ext_type(pi.arg_name, pi.arg_t),
                body,
                pi.dep_t(fresh))
            } yield ()
          case _ =>
            Left(Err(
              s"[check fail]\n" ++
                s"exp: ${pretty_exp(exp)}\n" ++
                s"t: ${pretty_val(t)}\n"))
        }
      case Ap(target: Exp, arg: Exp) =>
        infer(seed, env, ctx, target) match {
          case Left(err) => Left(err)
          case Right(pi: ValPi) =>
            check(seed, env, ctx, arg, pi.arg_t)
          case Right(_) =>
            Left(Err(
              s"[check fail]\n" ++
                s"exp: ${pretty_exp(exp)}\n" ++
                s"t: ${pretty_val(t)}\n"))
        }
      case Choice(path: List[String], map: Map[String, Exp]) =>
        ???
      case Dot(target: Exp, field_name: String) =>
        ???
      case DotType(target: Exp, field_name: String) =>
        ???
      case Let(decl: Decl, body: Exp) =>
        ???
    }
  }

  def infer(seed: Seed, env: Env, ctx: Ctx, exp: Exp): Either[Err, Val] = {
    ???
  }

  def check_level(seed: Seed, env: Env, ctx: Ctx, exp: Exp, level: Int): Either[Err, Unit] = {
    ???
  }

}
