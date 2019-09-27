package xieyuheng.cicada

import pretty._
import readback._
import fulfill._

object join {

  def join_val(x: Val, y: Val): Either[Err, Val] = {
    (x, y) match {
      // TODO we need meet_val and join_clo to implement this
      // case (x: ValPi, y: ValPi) =>
      //   for {
      //     _ <- meet_val(y.arg_t, x.arg_t)
      //     _ <- join_clo(x.dep_t, y.dep_t)
      //   } yield ()
      case (x: ValClub, y: ValClub) =>
        if (x.name == y.name) {
          for {
            new_tel <- merge_tel(x.tel, y.tel)
          } yield x.copy(tel = new_tel)
        } else {
          Left(Err(
            s"[join_val fail]\n" ++
              s"x: ${pretty_val(x)}\n" ++
              s"y: ${pretty_val(y)}"))
        }
      case (x: ValMember, y: ValMember) =>
        if (x.name == y.name) {
          for {
            new_tel <- merge_tel(x.tel, y.tel)
          } yield x.copy(tel = new_tel)
        } else {
          Left(Err(
            s"[join_val fail]\n" ++
              s"x: ${pretty_val(x)}\n" ++
              s"y: ${pretty_val(y)}"))
        }
      case (x: ValClub, y: ValMember) =>
        join_val(y, x)
      case (x: ValMember, y: ValClub) =>
        if (x.club_name == y.name) {
          for {
            new_tel <- merge_tel(x.tel, y.tel)
          } yield x.copy(tel = new_tel)
        } else {
          Left(Err(
            s"[join_val fail]\n" ++
              s"x: ${pretty_val(x)}\n" ++
              s"y: ${pretty_val(y)}"))
        }
      case (x, ValType(level)) =>
        join_type(x, level)
      case _ =>
        Left(Err(
          s"[join_val fail]\n" ++
            s"x: ${pretty_val(x)}\n" ++
            s"y: ${pretty_val(y)}"))
    }
  }

  def merge_tel(x: Tel, y: Tel): Either[Err, Tel] = {
    var new_fields: List[(String, Exp, Option[Exp], Option[Val], Option[Val])] = List()
    var maybe_err: Option[Err] = None

    x.fields.foreach { case (k, te, mve, mtv, mvv) =>
      y.fields.find { case (k2, te2, mve2, mtv2, mvv2) => k == k2 } match {
        case Some((k2, te2, mve2, mtv2, mvv2)) =>
          merge_mm(mtv, mvv, mtv2, mvv2) match {
            case Right((mtv, mvv)) =>
              new_fields = new_fields :+ (k, te, mve, mtv, mvv)
            case Left(err) =>
              maybe_err = Some(err)
          }
        case None =>
          new_fields = new_fields :+ (k, te, mve, mtv, mvv)
      }
    }
    maybe_err match {
      case Some(err) =>
        Left(err)
      case None =>
        Right(x.copy(fields = new_fields))
    }
  }

  def merge_mm(
    mtv: Option[Val],
    mvv: Option[Val],
    mtv2: Option[Val],
    mvv2: Option[Val],
  ): Either[Err, (Option[Val], Option[Val])] = {
    for {
      mtv <- merge_m(mtv, mtv2)
      mvv <- merge_m(mvv, mvv2)
    } yield (mtv, mvv)
  }

  def merge_m(
    mx: Option[Val],
    my: Option[Val],
  ): Either[Err, Option[Val]] = {
    (mx, my) match {
      case (Some(x), Some(y)) =>
        join_val(x, y) match {
          case Right(z) => Right(Some(z))
          case Left(err) => Left(err)
        }
      case (None, Some(y)) =>
        Right(Some(y))
      case (Some(x), None) =>
        Right(Some(x))
      case (None, None) =>
        Right(None)
    }
  }

  def join_type(x: Val, level: Int): Either[Err, Val] = {
    fulfill_type(x, level) match {
      case Right(()) => Right(x)
      case Left(err) => Left(err)
    }
  }

}
