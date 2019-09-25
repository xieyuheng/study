package xieyuheng.cicada

import pretty._

object fulfill {

  def fulfill_norm(x: Norm, y: Norm): Either[Err, Unit] = {
    (x, y) match {
      case (x: NormPi, y: NormPi) =>
        for {
          _ <- fulfill_norm(y.arg_t, x.arg_t)
          _ <- fulfill_norm(x.dep_t, y.dep_t)
        } yield (())
      case (x: NormFn, y: NormFn) =>
        for {
          _ <- fulfill_norm(y.arg_t, x.arg_t)
          _ <- fulfill_norm(x.body, y.body)
        } yield (())
      case (x: NormFn, y: NormPi) =>
        for {
          _ <- fulfill_norm(y.arg_t, x.arg_t)
          _ <- fulfill_norm(x.body, y.dep_t)
        } yield (())
      case (x: NormClub, y: NormClub) =>
        if (x.name == y.name) {
          fulfill_norm_tel(x.norm_tel, y.norm_tel)
        } else {
          Left(Err(
            s"[fulfill_norm fail]" ++
              s"x: ${pretty_norm(x)}" ++
              s"y: ${pretty_norm(y)}"))
        }
      case (x: NormMember, y: NormMember) =>
        if (x.name == y.name) {
          fulfill_norm_tel(x.norm_tel, y.norm_tel)
        } else {
          Left(Err(
            s"[fulfill_norm fail]" ++
              s"x: ${pretty_norm(x)}" ++
              s"y: ${pretty_norm(y)}"))
        }
      case (x: NormMember, y: NormClub) =>
        if (x.club_name == y.name) {
          fulfill_norm_tel(x.norm_tel, y.norm_tel)
        } else {
          Left(Err(
            s"[fulfill_norm fail]" ++
              s"x: ${pretty_norm(x)}" ++
              s"y: ${pretty_norm(y)}"))
        }
      case (x: NormRecord, y: NormRecord) =>
        if (x.name == y.name) {
          fulfill_norm_tel(x.norm_tel, y.norm_tel)
        } else {
          // TODO handle extends
          Left(Err(
            s"[fulfill_norm fail]" ++
              s"x: ${pretty_norm(x)}" ++
              s"y: ${pretty_norm(y)}"))
        }
      case (x: NormNeu, NormType(level)) =>
        fulfill_type(x, level)
      case (x: NormNeu, y: NormNeu) =>
        fulfill_norm_neu(x, y)
      case _ =>
        Left(Err(
          s"[fulfill_norm fail]" ++
            s"x: ${pretty_norm(x)}" ++
            s"y: ${pretty_norm(y)}"))
    }
  }

  def fulfill_norm_tel(x: NormTelescope, y: NormTelescope): Either[Err, Unit] = {
    ???
  }

  def fulfill_type(x: Norm, level: Int): Either[Err, Unit] = {
    x match {
      case x: NormType =>
        if (x.level <= level) {
          Right(())
        } else {
          Left(Err(
            s"[fulfill_type fail]" ++
              s"x: ${pretty_norm(x)}" ++
              s"level: ${level}"))
        }
      case x: NormPi =>
        for {
          _ <- fulfill_type(x.arg_t, level)
          _ <- fulfill_type(x.dep_t, level)
        } yield ()
      case x: NormFn =>
        for {
          _ <- fulfill_type(x.arg_t, level)
          _ <- fulfill_type(x.body, level)
        } yield ()
      case x: NormClub =>
        fulfill_norm_tel_type(x.norm_tel, level)
      case x: NormMember =>
        fulfill_norm_tel_type(x.norm_tel, level)
      case x: NormRecord =>
        fulfill_norm_tel_type(x.norm_tel, level)
      case _ =>
        Left(Err(
          s"[fulfill_type fail]" ++
            s"x: ${pretty_norm(x)}" ++
            s"level: ${level}"))
    }
  }

  def fulfill_norm_tel_type(x: NormTelescope, level: Int): Either[Err, Unit] = {
    ???
  }

  def fulfill_norm_neu(x: NormNeu, y: NormNeu): Either[Err, Unit] = {
    ???
  }

}
