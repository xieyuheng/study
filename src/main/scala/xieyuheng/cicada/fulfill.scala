package xieyuheng.cicada

import pretty._

object fulfill {

  def fulfill_norm(x: Norm, y: Norm): Either[Err, Unit] = {
    (x, y) match {
      case (x: NormPi, y: NormPi) =>
        for {
          _ <- fulfill_norm(y.arg_t, x.arg_t)
          _ <- fulfill_norm(x.dep_t, y.dep_t)
        } yield ()
      case (x: NormFn, y: NormFn) =>
        for {
          _ <- fulfill_norm(y.arg_t, x.arg_t)
          _ <- fulfill_norm(x.body, y.body)
        } yield ()
      case (x: NormFn, y: NormPi) =>
        for {
          _ <- fulfill_norm(y.arg_t, x.arg_t)
          _ <- fulfill_norm(x.body, y.dep_t)
        } yield ()
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
      case (x: NormNeu, y) =>
        infer_norm_neu(x).flatMap { case x => fulfill_norm(x, y) }
      case (x, y: NormNeu) =>
        infer_norm_neu(y).flatMap { case y => fulfill_norm(x, y) }
      case _ =>
        Left(Err(
          s"[fulfill_norm fail]" ++
            s"x: ${pretty_norm(x)}" ++
            s"y: ${pretty_norm(y)}"))
    }
  }

  def infer_norm_neu(norm_neu: NormNeu): Either[Err, Norm] = {
    norm_neu match {
      case NormNeuVar(name: String, norm_arg_t: Norm) =>
        Right(norm_arg_t)
      case NormNeuAp(target: NormNeu, arg: Norm) =>
        ???
      case NormNeuChoice(target: NormNeu, map: Map[String, Exp], env: NormEnv) =>
        // TODO to return list here
        ???
      case NormNeuDot(target: NormNeu, field_name: String) =>
        ???
      case NormNeuDotType(target: NormNeu, field_name: String) =>
        // TODO
        ???
    }
  }

  def fulfill_norm_tel(x: NormTelescope, y: NormTelescope): Either[Err, Unit] = {
    val list = y.fields.map { case (k, te, mve, tn, mvn) =>
      x.fields.find { case (k2, _, _, _, _) => k == k2 } match {
        case Some((k2, te2, mve2, tn2, mvn2)) =>
          (mvn2, mvn) match {
            case (Some(vn2), Some(vn)) =>
              fulfill_norm(tn2, tn).flatMap { _ => fulfill_norm(vn2, vn) }
            case (None, None) =>
              fulfill_norm(tn2, tn)
            case _ =>
              Left(Err(
                s"[fulfill_norm_tel fail]" ++
                  s"x: ${pretty_norm_tel(x)}" ++
                  s"y: ${pretty_norm_tel(y)}"))
          }
        case None =>
          Left(Err(
            s"[fulfill_norm_tel fail]" ++
              s"x: ${pretty_norm_tel(x)}" ++
              s"y: ${pretty_norm_tel(y)}"))
      }
    }

    val list2 = list :+ fulfill_norm_env(x.env, y.env)

    list2.find {
      case Right(_) => false
      case Left(_) => true
    } match {
      case Some(left) => left
      case None => Right(())
    }
  }

  def fulfill_norm_env(x: NormEnv, y: NormEnv): Either[Err, Unit] = {
    y match {
      case NormEnvDecl(decl: Decl, rest: NormEnv) =>
        fulfill_norm_env(x, rest)
      case NormEnvVal(name: String, value: Norm, rest: NormEnv) =>
        // NOTE x.find_norm only find NormEnvVal in x
        //   maybe we need x.lookup_norm
        //   which also lookup NormEnvDecl in x
        x.find_norm(name) match {
          case Some(v2) =>
            fulfill_norm(v2, value).flatMap { _ => fulfill_norm_env(x, rest) }
          case None =>
            Left(Err(
              s"[fulfill_norm_env fail]" ++
                s"missing name: ${name}"))
        }
      case NormEnvEmpty() =>
        Right(())
    }
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
    val list = x.fields.map { case (k, te, mve, tn, mvn) => fulfill_type(tn, level) }

    list.find {
      case Right(_) => false
      case Left(_) => true
    } match {
      case Some(left) => left
      case None => Right(())
    }
  }

}
