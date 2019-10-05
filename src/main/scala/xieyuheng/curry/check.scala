package xieyuheng.curry

import pretty._

import scala.annotation.tailrec

case class Ctx(map: Map[String, Type])

object check {

  def lookup_type(ctx: Ctx, name: String): Option[Type] = {
    ctx.map.get(name)
  }

  def ctx_ext_name(ctx: Ctx, name: String, t: Type): Ctx = {
    Ctx(ctx.map + (name -> t))
  }

  def check(ctx: Ctx, exp: Exp, t: Type): Either[Err, Unit] = {
    exp match {
      case Var(name: String) =>
        lookup_type(ctx, name) match {
          case Some(t2) =>
            if (t == t2) {
              Right(())
            } else {
              Left(Err(
                s"[check fail]\n" ++
                  s"variable name: ${name}\n" ++
                  s"found type: ${pretty_type(t2)}\n" ++
                  s"excepting type: ${pretty_type(t)}\n"
              ))
            }
          case None =>
            Left(Err(
              s"[check fail]\n" ++
                s"undefined name: ${name}\n" ++
                s"excepting type: ${pretty_type(t)}\n"
            ))
        }
      case Ap(target: Exp, arg: Exp) =>
        infer(ctx, target) match {
          case Left(err) =>
            Left(Err(
              "[check fail]\n" ++
                s"exp: ${pretty_exp(exp)}\n" ++
                s"excepting type: ${pretty_type(t)}\n"
            ).append_cause(err))
          case Right(TypeAtom(name)) =>
            Left(Err(
              "[check fail]\n" ++
                s"exp: ${pretty_exp(exp)}\n" ++
                s"excepting type: ${pretty_type(t)}\n" ++
                s"target: ${pretty_exp(target)}\n" ++
                s"excepting target type to be type-arrow\n" ++
                s"infered target type is type-atom: ${name}\n"
            ))
          case Right(TypeArrow(arg_t, ret_t)) =>
            check(ctx, arg, arg_t)
        }
      case Fn(arg_name: String, body: Exp) =>
        t match {
          case TypeAtom(name) =>
            Left(Err(
              "[check fail]\n" ++
                s"exp: ${pretty_exp(exp)}\n" ++
                s"excepting type: ${pretty_type(t)}\n" ++
                s"excepting type-arrow\n" ++
                s"but found type-atom: ${name}\n"
            ))
          case TypeArrow(arg_t, ret_t) =>
            check(ctx_ext_name(ctx, arg_name, arg_t), body, ret_t)
        }
    }
  }

  def infer(ctx: Ctx, exp: Exp): Either[Err, Type] = {
    exp match {
      case Var(name: String) =>
        ???
      case Ap(target: Exp, arg: Exp) =>
        ???
      case Fn(arg_name: String, body: Exp) =>
        ???
    }
  }

}
