package xieyuheng.adventure.jojo_simple

import cut._
import pretty._

import xieyuheng.util.err._

import scala.annotation.tailrec

object exe {

  def frame_empty: Frame = {
    Frame(0, List(), Env(), Ctx())
  }

  def run(ds: Ds, rs: Rs): Either[Err, Ds] = {
    run_with_limit(ds, rs, 0) match {
      case Right((ds, rs)) => Right(ds)
      case Left(err) => Left(err)
    }
  }

  def run_jo_list(ds: Ds, rs: Rs, list: List[Jo]): Either[Err, (Ds, Rs)] = {
    val limit = rs.length
    val frame = Frame(0, list, Env(), Ctx())
    run_with_limit(ds, rs.push(frame), limit)
  }

  @tailrec
  def run_with_limit(ds: Ds, rs: Rs, limit: Int): Either[Err, (Ds, Rs)] = {
    if (rs.length > limit) {
      step(ds, rs) match {
        case Right((ds, rs)) =>
          run_with_limit(ds, rs, limit)
        case Left(err) =>
          Left(err)
      }
    } else {
      Right(ds, rs)
    }
  }

  def step(ds: Ds, rs: Rs): Either[Err, (Ds, Rs)] = {
    rs.toc() match {
      case Some(frame) =>
        if (frame.index == frame.list.length) {
          Right(ds, rs.drop())
        } else {
          val jo = frame.list(frame.index)
          exe(ds, rs.next(), frame.env, frame.ctx, jo)
        }
      case None =>
        Left(Err(
          s"[step fail]\n" ++
            s"stack underflow\n" ++
            s"${pretty_rs(rs)}\n" ++
            s"${pretty_ds(ds)}\n"
        ))
    }
  }

  def exe(ds: Ds, rs: Rs, env: Env, ctx: Ctx, jo: Jo): Either[Err, (Ds, Rs)] = {
    jo match {

      case Var(name: String) =>
        env.lookup(name) match {
          case Some(EnvEntryDefine(value)) =>
            ap(ds, rs, env, value)
          case Some(EnvEntryLet(value)) =>
            Right(ds.push(value), rs)
          case None =>
            Left(Err(
              s"[exe fail]\n" ++
                s"undefined name: ${name}\n" ++
                s"${pretty_rs(rs)}\n" ++
                s"${pretty_ds(ds)}\n"
            ))
        }

      case Let(name: String, tyty: TyTy) =>
        ds.toc() match {
          case Some(value) =>
            val new_rs = rs
              .toc_ext_env(name, EnvEntryLet(value))
              .toc_ext_ctx(name, CtxEntryLet(tyty))
            Right(ds.drop(), new_rs)
          case None =>
            Left(Err(
              s"[exe fail]\n" ++
                s"stack underflow\n" ++
                s"${pretty_rs(rs)}\n" ++
                s"${pretty_ds(ds)}\n"
            ))
        }

      case JoJo(list: List[Jo]) =>
        Right(ds.push(ValJoJo(list, env, ctx)), rs)

      case Claim(name: String, tyty: TyTy) =>
        val new_rs = rs
          .toc_ext_ctx(name, CtxEntryClaim(tyty))
        Right(ds, new_rs)

      case Define(name: String, jojo: JoJo) =>
        ctx.lookup(name) match {
          case Some(CtxEntryClaim(tyty: TyTy)) =>
            cut_jojo(jojo, ctx).flatMap { case infered_tyty =>
              reduce_tyty(tyty).flatMap { case tyty2 =>
                reduce_tyty(infered_tyty).flatMap { case infered_tyty2 =>
                  if (tyty2 == infered_tyty2) {
                    Right(ds, rs.toc_ext_env(name, EnvEntryDefine(ValJoJo(jojo.list, env, ctx))))
                  } else {
                    Left(Err(
                      s"[exe fail]\n" ++
                        s"can not define ${name}\n" ++
                        s"the claimed type\n" ++
                        s">>> ${pretty_ty(tyty)}\n" ++
                        s"=== ${pretty_ty(tyty2)}\n" ++
                        s"is not equal to the infered type\n" ++
                        s">>> ${pretty_ty(infered_tyty)}\n" ++
                        s"=== ${pretty_ty(infered_tyty2)}\n" ++
                        s"${pretty_rs(rs)}\n" ++
                        s"${pretty_ds(ds)}\n"
                    ))
                  }
                }
              }
            }
          case _ =>
            Left(Err(
              s"[exe fail]\n" ++
                s"can not define ${name}\n" ++
                s"the name is not claimed\n" ++
                s"${pretty_rs(rs)}\n" ++
                s"${pretty_ds(ds)}\n"
            ))
        }

      case Execute() =>
        ds.toc() match {
          case Some(jojo: ValJoJo) =>
            Right(ds.drop(), rs.push(Frame(0, jojo.list, jojo.env, jojo.ctx)))
          case Some(value) =>
            Left(Err(
              s"[exe fail]\n" ++
                s"exe type mismatch match, excepting jojo\n" ++
                s"value: ${pretty_val(value)}\n" ++
                s"${pretty_rs(rs)}\n" ++
                s"${pretty_ds(ds)}\n"
            ))
          case None =>
            Left(Err(
              s"[exe fail]\n" ++
                s"stack underflow\n" ++
                s"${pretty_rs(rs)}\n" ++
                s"${pretty_ds(ds)}\n"
            ))
        }

      case AssertEq() =>
        ds.toc() match {
          case Some(x) =>
            ds.drop().toc() match {
              case Some(y) =>
                if (x == y) {
                  Right(ds.drop().drop(), rs)
                } else {
                  Left(Err(
                    s"[assert_eq fail]\n" ++
                      s">>> ${pretty_val(x)}\n" ++
                      s"=/= ${pretty_val(y)}\n" ++
                      s"${pretty_rs(rs)}\n" ++
                      s"${pretty_ds(ds)}\n"
                  ))
                }
              case None =>
                Left(Err(
                  s"[exe fail]\n" ++
                    s"stack underflow\n" ++
                    s"${pretty_rs(rs)}\n" ++
                    s"${pretty_ds(ds)}\n"
                ))
            }
          case None =>
            Left(Err(
              s"[exe fail]\n" ++
                s"stack underflow\n" ++
                s"${pretty_rs(rs)}\n" ++
                s"${pretty_ds(ds)}\n"
            ))
        }

      case ReportDs() =>
        println(pretty_ds(ds))
        Right(ds, rs)

      case ReportRs() =>
        println(pretty_rs(rs))
        Right(ds, rs)

      case Print() =>
        ds.toc() match {
          case Some(value) =>
            print(pretty_val(value))
            Right(ds.drop(), rs)
          case None =>
            Left(Err(
              s"[exe fail]\n" ++
                s"stack underflow\n" ++
                s"${pretty_rs(rs)}\n" ++
                s"${pretty_ds(ds)}\n"
            ))
        }

      case Newline() =>
        println("")
        Right(ds, rs)

    }
  }

  def ap(ds: Ds, rs: Rs, env: Env, value: Val): Either[Err, (Ds, Rs)] = {
    value match {
      case jojo: ValJoJo =>
        Right(ds, rs.push(Frame(0, jojo.list, jojo.env, jojo.ctx)))
      case value =>
        Right(ds.push(value), rs)
    }
  }

}
