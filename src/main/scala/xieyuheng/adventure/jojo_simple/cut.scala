package xieyuheng.adventure.jojo_simple

import pretty._

import xieyuheng.util.err._

import scala.annotation.tailrec

object cut {

  def cut_jojo(jojo: JoJo, ctx: Ctx): Either[Err, TyTy] = {
    val init_result: Either[Err, List[Ty]] = Right(List())
    val result: Either[Err, List[Ty]]  =
      jojo.list.foldLeft(init_result) { case (result, jo) =>
        result.flatMap { case ty_list =>
          cut_jo(jo, ctx) match {
            case Right(ty_list2) =>
              Right(ty_list ++ ty_list2)
            case Left(err) =>
              Left(err)
          }
        }
      }
    result match {
      case Right(ty_list) =>
        Right(TyTy(ty_list))
      case Left(err) =>
        Left(Err(
          s"[cut_jojo fail]\n" ++
            s"jojo: ${pretty_jo(jojo)}\n"
        ).append_cause(err))
    }
  }

  def cut_jo(jo: Jo, ctx: Ctx): Either[Err, List[Ty]] = {
    jo match {
      case Var(name: String) =>
        ctx.lookup(name) match {
          case Some(CtxEntryLet(tyty)) =>
            Right(tyty.list)
          case Some(CtxEntryClaim(tyty)) =>
            Right(tyty.list)
          case None =>
            Left(Err(
              s"[cut_jo fail]\n" ++
                s"un-claimed name: ${name}\n"
            ))
        }
      case Let(name: String, tyty: TyTy) =>
        Right(tyty.list)
      case jojo: JoJo =>
        cut_jojo(jojo, ctx).flatMap {
          case tyty =>
            Right(List(tyty))
        }
      case Claim(name: String, tyty: TyTy) =>
        Right(List())
      case Define(name: String, jojo: JoJo) =>
        Right(List())
      case Execute() =>
        Right(List(TyCut()))
      case AssertEq() =>
        Right(List(TyAssertEq()))
      case ReportDs() =>
        Right(List())
      case ReportRs() =>
        Right(List())
      case Print() =>
        Right(List(TyPrint()))
      case Newline() =>
        Right(List())
    }
  }

  def step_tyty(tyty: TyTy): Either[Err, TyTy] = {
    val init_result: Either[Err, Ts] = Right(Ts())
    val result: Either[Err, Ts] =
      tyty.list.foldLeft(init_result) { case (result, ty) =>
        result.flatMap { case ts =>
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
                        s"t in (- t): ${pretty_ty(t)}\n" ++
                        s"t to match: ${pretty_ty(t2)}\n"
                    ))
                  }
                case Some(t2: TyTy) =>
                  if (t == t2) {
                    Right(ts.drop())
                  } else {
                    Left(Err(
                      s"[step_tyty fail]\n" ++
                        s"(- ...) mismatch\n" ++
                        s"t in (- t): ${pretty_ty(t)}\n" ++
                        s"t to match: ${pretty_ty(t2)}\n"
                    ))
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
      }
    result match {
      case Right(ts) =>
        Right(TyTy(ts.list.reverse))
      case Left(err) =>
        Left(Err(
          s"[step_tyty fail]\n" ++
            s"tyty: ${pretty_ty(tyty)}\n"
        ).append_cause(err))
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
