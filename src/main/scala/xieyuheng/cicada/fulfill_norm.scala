package xieyuheng.cicada

import pretty._
import readback._

object fulfill_norm {

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
        val list = infer_norm_neu(x).map {
          case result =>
            result.flatMap { case x => fulfill_norm(x, y) } }
        first_err(list)
      case (x, y: NormNeu) =>
        val list = infer_norm_neu(y).map {
          case result =>
            result.flatMap { case y => fulfill_norm(x, y) } }
        first_err(list)
      case _ =>
        Left(Err(
          s"[fulfill_norm fail]" ++
            s"x: ${pretty_norm(x)}" ++
            s"y: ${pretty_norm(y)}"))
    }
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

  def infer_norm_neu(norm_neu: NormNeu): List[Either[Err, Norm]] = {
    norm_neu match {
      case NormNeuVar(name: String, norm_arg_t: Norm) =>
        List(Right(norm_arg_t))
      case NormNeuAp(target: NormNeu, arg: Norm) =>
        infer_norm_neu(target).flatMap {
          _ match {
            case Left(err) =>
              List(Left(err))
            case Right(NormPi(arg_name: String, arg_t: Norm, dep_t: Norm)) =>
              List(fulfill_norm(arg, arg_t).flatMap { _ => Right(dep_t) })
            case Right(NormFn(arg_name: String, arg_t: Norm, body: Norm)) =>
              List(fulfill_norm(arg, arg_t).flatMap { _ => Right(body) })
            case Right(NormClub(name: String, members: List[Member], norm_tel: NormTel)) =>
              List(norm_tel.put(arg).flatMap {
                case new_norm_tel =>
                  Right(NormClub(name: String, members: List[Member], new_norm_tel)) })
            case Right(NormMember(name: String, club_name: String, norm_tel: NormTel)) =>
              List(norm_tel.put(arg).flatMap {
                case new_norm_tel =>
                  Right(NormMember(name: String, club_name: String, new_norm_tel)) })
            case Right(NormRecord(name: String, super_names: List[String], norm_tel: NormTel)) =>
              List(norm_tel.put(arg).flatMap {
                case new_norm_tel =>
                  Right(NormRecord(name: String, super_names: List[String], new_norm_tel)) })
            case Right(_) =>
              List(Left(Err(
                s"[infer_norm_neu fail]" ++
                  s"norm_neu: ${pretty_norm(norm_neu)}")))
          }
        }
      case NormNeuChoice(target, path, map, seed, env) =>
        infer_norm_neu(target).flatMap {
          case Right(norm) =>
            map.toList.map { case (choice_name, body) =>
              refine_choice(norm, choice_name, body, path, seed, env) }
          case Left(err) => List(Left(err)) }
      case NormNeuDot(target: NormNeu, field_name: String) =>
        infer_norm_neu(target).flatMap {
          _ match {
            case Left(err) =>
              List(Left(err))
            case Right(NormClub(name: String, members: List[Member], norm_tel: NormTel)) =>
              List(infer_norm_tel_dot(norm_tel: NormTel, field_name: String))
            case Right(NormMember(name: String, club_name: String, norm_tel: NormTel)) =>
              List(infer_norm_tel_dot(norm_tel: NormTel, field_name: String))
            case Right(NormRecord(name: String, super_names: List[String], norm_tel: NormTel)) =>
              List(infer_norm_tel_dot(norm_tel: NormTel, field_name: String))
            case Right(_) =>
              List(Left(Err(
                s"[infer_norm_neu fail]" ++
                  s"norm_neu: ${pretty_norm(norm_neu)}")))
          }
        }
      case NormNeuDotType(target: NormNeu, field_name: String) =>
        // TODO
        ???
    }
  }

  def refine_choice(
    norm: Norm,
    choice_name: String,
    body: Exp,
    path: List[String],
    seed: Seed,
    env: Env,
  ): Either[Err, Norm] = {
    env.lookup_val(choice_name) match {
      case Some(value) =>
        for {
          refined_val <- join_norm_with_val(norm, value)
          refined_env <- env.ext_by_path(path, refined_val)
        } yield readback_val(seed, eval(body, refined_env))
      case None =>
        Left(Err(
          s"[refine_choice fail]" ++
            s"norm: ${pretty_norm(norm)}" ++
            s"path: ${pretty_path(path)}" ++
            s"choice_name: ${choice_name}" ++
            s"body: ${pretty_exp(body)}"))
    }
  }

  def join_norm_with_val(norm: Norm, value: Val): Either[Err, Val] = {
    (norm, value) match {
      case (norm: NormClub, value: ValMember) =>
        if (norm.name == value.club_name) {
          for {
            new_tel <- join_norm_tel_with_val_tel(norm.norm_tel, value.tel)
          } yield value.copy(tel = new_tel)
        } else {
          Left(Err(
            s"[join_norm_with_val fail]" ++
              s"norm: ${pretty_norm(norm)}" ++
              s"value: ${pretty_val(value)}"))
        }
      case (norm: NormMember, value: ValMember) =>
        if (norm.name == value.name) {
          for {
            new_tel <- join_norm_tel_with_val_tel(norm.norm_tel, value.tel)
          } yield value.copy(tel = new_tel)
        } else {
          Left(Err(
            s"[join_norm_with_val fail]" ++
              s"norm: ${pretty_norm(norm)}" ++
              s"value: ${pretty_val(value)}"))
        }
      case _ =>
        Left(Err(
          s"[join_norm_with_val fail]" ++
            s"norm: ${pretty_norm(norm)}" ++
            s"value: ${pretty_val(value)}"))
    }
  }

  def join_norm_tel_with_val_tel(
    norm_tel: NormTel,
    tel: Tel,
  ): Either[Err, Tel] = {
    ???
  }

  def infer_norm_tel_dot(norm_tel: NormTel, field_name: String): Either[Err, Norm] = {
    norm_tel.fields.find {
      case (k, _, _, _, _) =>
        k == field_name
    } match {
      case Some((_, _, _, tn, _)) =>
        Right(tn)
      case None =>
        Left(Err(
          s"[infer_norm_tel_dot fail]" ++
            s"norm_tel: ${pretty_norm_tel(norm_tel)}" ++
            s"field_name: ${field_name}"))
    }
  }

  def fulfill_norm_tel(x: NormTel, y: NormTel): Either[Err, Unit] = {
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

    first_err(list)
  }

  def fulfill_type(x: Norm, level: Int): Either[Err, Unit] = {
    x match {
      case x: NormType =>
        if (x.level < level) {
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

  def fulfill_norm_tel_type(x: NormTel, level: Int): Either[Err, Unit] = {
    val list = x.fields.map { case (k, te, mve, tn, mvn) => fulfill_type(tn, level) }
    first_err(list)
  }

}
