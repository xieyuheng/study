package xieyuheng.adventure.jojo_simple

import pretty._

import xieyuheng.util.err._

import scala.annotation.tailrec

object cut {

  def cut_jojo(jojo: JoJo, ctx: Ctx): Either[Err, TyTy] = {
    val init_result: Either[Err, (Ctx, List[Ty])] = Right(ctx, List())
    val result: Either[Err, (Ctx, List[Ty])]  =
      jojo.list.foldLeft(init_result) { case (result, jo) =>
        result.flatMap { case (ctx, ty_list) =>
          cut_jo(jo, ctx) match {
            case Right((ctx2, ty_list2)) =>
              Right(ctx2, ty_list ++ ty_list2)
            case Left(err) =>
              Left(err)
          }
        }
      }
    result match {
      case Right((ctx, ty_list)) =>
        Right(TyTy(ty_list))
      case Left(err) =>
        Left(Err(
          s"[cut_jojo fail]\n" ++
            s"jojo: ${pretty_jo(jojo)}\n"
        ).cause(err))
    }
  }

  def cut_jo(jo: Jo, ctx: Ctx): Either[Err, (Ctx, List[Ty])] = {
    jo match {

      case Var(name: String) =>
        ctx.lookup(name) match {
          case Some(CtxEntryLet(t)) =>
            Right(ctx, List(t))
          case Some(CtxEntryClaim(tyty)) =>
            Right(ctx, tyty.list)
          case None =>
            Left(Err(
              s"[cut_jo fail]\n" ++
                s"un-claimed name: ${name}\n"
            ))
        }

      case Let(name: String, t: Ty) =>
        Right(ctx.ext(name, CtxEntryLet(t)), List(TyMinus(t)))

      case jojo: JoJo =>
        cut_jojo(jojo, ctx).flatMap {
          case tyty =>
            Right(ctx, List(tyty))
        }

      case Claim(name: String, tyty: TyTy) =>
        Right(ctx, List())

      case Define(name: String, jojo: JoJo) =>
        Right(ctx, List())

      case Execute() =>
        Right(ctx, List(TyCut()))

      case AssertEq() =>
        Right(ctx, List(TyAssertEq()))

      case ReportDs() =>
        Right(ctx, List())

      case ReportRs() =>
        Right(ctx, List())

      case Print() =>
        Right(ctx, List(TyPrint()))

      case Newline() =>
        Right(ctx, List())

      case Atom(name, str) =>
        Right(ctx, List(TyAtom(name)))

    }
  }

  def step_ty_in_ts(ts: Ts, ty: Ty): Either[Err, Ts] = {
    ty match {

      case TyAtom(name: String) =>
        Right(ts.push(ty))

      case TyTy(list: List[Ty]) =>
        Right(ts.push(ty))

      case TyCut() =>
        ts.toc() match {
          case Some(t2: TyAtom) =>
            Left(Err(
              s"[step_tyty fail]\n" ++
                s"cut type mismatch\n" ++
                s"t to match: ${pretty_ty(t2)}\n"
            ))
          case Some(tyty: TyTy) =>
            Right(ts.drop().push_list(tyty.list))
          case Some(t2) =>
            Right(ts.push(ty))
          case None =>
            Right(ts.push(ty))
        }

      case TyMinus(t: Ty) =>
        ts.toc() match {
          case Some(t2: TyAtom) =>
            if (t == t2) {
              Right(ts.drop())
            } else {
              Left(Err(
                s"[step_tyty fail]\n" ++
                  s"(- ...) mismatch\n" ++
                  s"t in (- t):\n" ++
                  s">>> ${pretty_ty(t)}\n" ++
                  s"=/= ${pretty_ty(t2)}\n"
              ))
            }
          case Some(tyty: TyTy) =>
            // NOTE
            //   the use of `reduce_tyty` here
            //   makes the `step_tyty` not one step
            //   but many steps
            reduce_tyty(tyty).flatMap { case tyty2 =>
              if (t == tyty2) {
                Right(ts.drop())
              } else {
                Left(Err(
                  s"[step_tyty fail]\n" ++
                    s"(- ...) mismatch\n" ++
                    s"t in (- t):\n" ++
                    s">>> ${pretty_ty(t)}\n" ++
                    s"=/= ${pretty_ty(tyty2)}\n"
                ))
              }
            }
          case Some(t2) =>
            Right(ts.push(ty))
          case None =>
            Right(ts.push(ty))
        }

      case TyAssertEq() =>
        // NOTE we enforc eq for the same type
        if (ts.length < 2) {
          Left(Err(
            s"[step_tyty fail]\n" ++
              s"ts underflow during ty_assert_eq\n"
          ))
        } else {
          // TODO
          Right(ts.drop().drop())
        }

      case TyPrint() =>
        if (ts.length < 1) {
          Left(Err(
            s"[step_tyty fail]\n" ++
              s"ts underflow during ty_print\n"
          ))
        } else {
          Right(ts.drop())
        }

    }
  }

  def step_tyty(tyty: TyTy): Either[Err, TyTy] = {
    val init_result: Either[Err, Ts] = Right(Ts())
    val result: Either[Err, Ts] =
      tyty.list.foldLeft(init_result) { case (result, ty) =>
        result.flatMap { case ts =>
          step_ty_in_ts(ts, ty)
        }
      }
    result match {
      case Right(ts) =>
        Right(TyTy(ts.list.reverse))
      case Left(err) =>
        Left(Err(
          s"[step_tyty fail]\n" ++
            s"tyty: ${pretty_ty(tyty)}\n"
        ).cause(err))
    }
  }

  def reduce_tyty(tyty: TyTy): Either[Err, TyTy] = {
    step_tyty(tyty).flatMap { case tyty2 =>
      if (tyty == tyty2) {
        Right(tyty)
      } else {
        reduce_tyty(tyty2)
      }
    }
  }

}
