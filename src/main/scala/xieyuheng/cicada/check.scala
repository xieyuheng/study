package xieyuheng.cicada

import pretty._
import readback._

object check {

  def apply(seed: Seed, env: Env, ctx: Ctx, exp: Exp, t: Val): Either[Err, Unit] =
    check(seed: Seed, env: Env, ctx: Ctx, exp: Exp, t: Val)

  def check(seed: Seed, env: Env, ctx: Ctx, exp: Exp, t: Val): Either[Err, Unit] = {
    exp match {
      case Var(name: String) =>
        ctx.lookup_type(name) match {
          case Some(u) =>
            subtype_val(u, t)
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
              _ <- subtype_val(pi.arg_t, art_t_val)
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
        first_err(map.toList.map { case (choice_name, body) =>
          check_choice(seed, env, ctx, path, choice_name, body, t) })
      case Dot(target: Exp, field_name: String) =>
        infer(seed, env, ctx, target) match {
          case Left(err) => Left(err)
          case Right(v: ValClub) =>
            subtype_val(v.tel.dot_type(field_name), t)
          case Right(v: ValMember) =>
            subtype_val(v.tel.dot_type(field_name), t)
          case Right(v: ValRecord) =>
            subtype_val(v.tel.dot_type(field_name), t)
          case Right(_) =>
            Left(Err(
              s"[check fail]\n" ++
                s"exp: ${pretty_exp(exp)}\n" ++
                s"t: ${pretty_val(t)}\n"))
        }
      case DotType(target: Exp, field_name: String) =>
        ???
      case Let(decl: Decl, body: Exp) =>
        ???
    }
  }

  def infer(seed: Seed, env: Env, ctx: Ctx, exp: Exp): Either[Err, Val] = {
    exp match {
      case Var(name: String) =>
        ctx.lookup_type(name) match {
          case Some(t) => Right(t)
          case None =>
            Left(Err(
              s"[infer fail]\n" ++
                s"exp: ${pretty_exp(exp)}\n"))
        }
      case Type(level: Int) =>
        Right(ValType(level + 1))
      case Pi(arg_name: String, arg_t: Exp, dep_t: Exp) =>
        for {
          x <- infer_level(seed, env, ctx, arg_t)
          y <- infer_level(seed, env, ctx, dep_t)
        } yield ValType(Math.max(x, y))
      case Fn(arg_name: String, arg_t: Exp, dep_t: Exp, body: Exp) =>
        val pi = Pi(arg_name, arg_t, dep_t)
        Right(eval(pi, env))
      case Ap(target: Exp, arg: Exp) =>
        infer(seed, env, ctx, target) match {
          case Left(err) => Left(err)
          case Right(pi: ValPi) =>
            for {
              _ <- check(seed, env, ctx, arg, pi.arg_t)
            } yield (pi.dep_t(eval(arg, env)))
          case Right(_) =>
            Left(Err(
              s"[infer fail]\n" ++
                s"exp: ${pretty_exp(exp)}\n"))
        }
      case Choice(path: List[String], map: Map[String, Exp]) =>
        Left(Err(
          s"[infer fail]\n" ++
            s"can not infer choice\n" ++
            s"exp: ${pretty_exp(exp)}\n"))
      case Dot(target: Exp, field_name: String) =>
        infer(seed, env, ctx, target) match {
          case Left(err) => Left(err)
          case Right(v: ValClub) =>
            Right(v.tel.dot_type(field_name))
          case Right(v: ValMember) =>
            Right(v.tel.dot_type(field_name))
          case Right(v: ValRecord) =>
            Right(v.tel.dot_type(field_name))
          case Right(_) =>
            Left(Err(
              s"[infer fail]\n" ++
                s"exp: ${pretty_exp(exp)}\n"))
        }
      case DotType(target: Exp, field_name: String) =>
        ???
      case Let(decl: Decl, body: Exp) =>
        ???
    }
  }

  def infer_level(seed: Seed, env: Env, ctx: Ctx, exp: Exp): Either[Err, Int] = {
    // TODO
    Right(1)
  }

  def check_level(seed: Seed, env: Env, ctx: Ctx, exp: Exp, level: Int): Either[Err, Unit] = {
    // TODO
    Right(())
  }

  def check_choice(
    seed: Seed, env: Env, ctx: Ctx,
    path: List[String], choice_name: String,
    body: Exp, t: Val,
  ): Either[Err, Unit] = {
    ???
  }

  def subtype_val(x: Val, y: Val): Either[Err, Unit] = {
    ???
  }

  def first_err(list: List[Either[Err, Unit]]): Either[Err, Unit] = {
    list.find {
      case Right(_) => false
      case Left(_) => true
    } match {
      case Some(left) => left
      case None => Right(())
    }
  }


}
