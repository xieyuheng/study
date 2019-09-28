package xieyuheng.cicada

import infer._
import pretty._
import readback._

object fulfill {

  def fulfill_val(x: Val, y: Val): Either[Err, Unit] = {
    (x, y) match {
      case (x: ValPi, y: ValPi) =>
        for {
          _ <- fulfill_val(y.arg_t, x.arg_t)
          _ <- fulfill_clo(x.dep_t, y.dep_t)
        } yield ()
      case (x: ValFn, y: ValFn) =>
        for {
          _ <- fulfill_val(y.arg_t, x.arg_t)
          _ = {
            println("------")
          }
          _ <- fulfill_clo(x.body, y.body)
        } yield ()
      case (x: ValFn, y: ValPi) =>
        for {
          _ <- fulfill_val(y.arg_t, x.arg_t)
          _ <- fulfill_clo(x.body, y.dep_t)
        } yield ()
      case (x: ValClub, y: ValClub) =>
        if (x.name == y.name) {
          fulfill_tel(x.tel, y.tel)
        } else {
          Left(Err(
            s"[fulfill_val fail]\n" ++
              s"x: ${pretty_val(x)}\n" ++
              s"y: ${pretty_val(y)}\n"))
        }
      case (x: ValMember, y: ValMember) =>
        if (x.name == y.name) {
          fulfill_tel(x.tel, y.tel)
        } else {
          Left(Err(
            s"[fulfill_val fail]\n" ++
              s"x: ${pretty_val(x)}\n" ++
              s"y: ${pretty_val(y)}\n"))
        }
      case (x: ValMember, y: ValClub) =>
        if (x.club_name == y.name) {
          fulfill_tel(x.tel, y.tel)
        } else {
          Left(Err(
            s"[fulfill_val fail]\n" ++
              s"x: ${pretty_val(x)}\n" ++
              s"y: ${pretty_val(y)}\n"))
        }
      case (x: ValRecord, y: ValRecord) =>
        if (x.name == y.name) {
          fulfill_tel(x.tel, y.tel)
        } else {
          // TODO handle extends
          Left(Err(
            s"[fulfill_val fail]\n" ++
              s"x: ${pretty_val(x)}\n" ++
              s"y: ${pretty_val(y)}\n"))
        }
      case (x, ValType(level)) =>
        fulfill_type(x, level)
      case (x: ValClub, y: ValPi) =>
        fields_find_first_none(x.tel.fields) match {
          case Some((k, te, mve, Some(tv), None)) =>
            for {
              _ <- fulfill_val(y.arg_t, tv)
              new_tel <- x.tel.put(gen_neu_val(k, tv, None))
            } yield fulfill_val(x.copy(tel = new_tel), y.dep_t.force())
          case Some((k, te, mve, None, None)) =>
            val tv = eval(te, x.tel.env)
            for {
              _ <- fulfill_val(y.arg_t, tv)
              new_tel <- x.tel.put(gen_neu_val(k, tv, None))
            } yield fulfill_val(x.copy(tel = new_tel), y.dep_t.force())
          case _ =>
            Left(Err(
              s"[fulfill_val fail]\n" ++
                s"x: ${pretty_val(x)}\n" ++
                s"y: ${pretty_val(y)}\n"))
        }
      case (x: ValMember, y: ValPi) =>
        fields_find_first_none(x.tel.fields) match {
          case Some((k, te, mve, Some(tv), None)) =>
            for {
              _ <- fulfill_val(y.arg_t, tv)
              new_tel <- x.tel.put(gen_neu_val(k, tv, None))
            } yield fulfill_val(x.copy(tel = new_tel), y.dep_t.force())
          case Some((k, te, mve, None, None)) =>
            val tv = eval(te, x.tel.env)
            for {
              _ <- fulfill_val(y.arg_t, tv)
              new_tel <- x.tel.put(gen_neu_val(k, tv, None))
            } yield fulfill_val(x.copy(tel = new_tel), y.dep_t.force())
          case _ =>
            Left(Err(
              s"[fulfill_val fail]\n" ++
                s"x: ${pretty_val(x)}\n" ++
                s"y: ${pretty_val(y)}\n"))
        }
      case (x: ValRecord, y: ValPi) =>
        fields_find_first_none(x.tel.fields) match {
          case Some((k, te, mve, Some(tv), None)) =>
            for {
              _ <- fulfill_val(y.arg_t, tv)
              new_tel <- x.tel.put(gen_neu_val(k, tv, None))
            } yield fulfill_val(x.copy(tel = new_tel), y.dep_t.force())
          case Some((k, te, mve, None, None)) =>
            val tv = eval(te, x.tel.env)
            for {
              _ <- fulfill_val(y.arg_t, tv)
              new_tel <- x.tel.put(gen_neu_val(k, tv, None))
            } yield fulfill_val(x.copy(tel = new_tel), y.dep_t.force())
          case _ =>
            Left(Err(
              s"[fulfill_val fail]\n" ++
                s"x: ${pretty_val(x)}\n" ++
                s"y: ${pretty_val(y)}\n"))
        }
      case (x: Neu, y) =>
        val list = infer_neu(x).map {
          case result =>
            result.flatMap { case x => fulfill_val(x, y) } }
        first_err(list)
      case (x, y: Neu) =>
        val list = infer_neu(y).map {
          case result =>
            result.flatMap { case y => fulfill_val(x, y) } }
        first_err(list)
      case _ =>
        Left(Err(
          s"[fulfill_val fail]\n" ++
            s"not handled case\n" ++
            s"x: ${pretty_val(x)}\n" ++
            s"y: ${pretty_val(y)}\n"))
    }
  }

  def fulfill_clo(x: Clo, y: Clo): Either[Err, Unit] = {
    fulfill_val(x.force(), y.force())
  }

  def fulfill_tel(x: Tel, y: Tel): Either[Err, Unit] = {
    val x_forced = x.force()
    val y_forced = y.force()

    val list = y_forced.fields.map {
      case (k, te, mve, Some(tv), Some(vv)) =>
        x_forced.fields.find { case (k2, _, _, _, _) => k == k2 } match {
          case Some((k2, te2, mve2, Some(tv2), Some(vv2))) =>
            fulfill_val(tv2, tv).flatMap { _ => fulfill_val(vv2, vv) }
          case Some(_) =>
            println(s"[internal error]")
            println(s"tel.force is not effective")
            throw new Exception()
          case None =>
            Left(Err(
              s"[fulfill_tel fail]\n" ++
                s"x: ${pretty_tel(x)}\n" ++
                s"y: ${pretty_tel(y)}\n"))
        }
      case _ =>
        println(s"[internal error]")
        println(s"tel.force is not effective")
        throw new Exception()
    }

    first_err(list)
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

  def fields_find_first_none(
    fields: List[(String, Exp, Option[Exp], Option[Val], Option[Val])]
  ): Option[(String, Exp, Option[Exp], Option[Val], Option[Val])] = {
    fields.find {
      case (k, te, mve, mtv, None) => true
      case (k, te, mve, mtv, Some(vv)) => false
    }
  }

  def fulfill_type(x: Val, level: Int): Either[Err, Unit] = {
    x match {
      case neu: Neu =>
        first_err(infer_neu(neu).map {
          // NOTE maybe need to dec level here
          case Right(t) => fulfill_type(t, level)
          case Left(err) => Left(err)
        })
      case x: ValType =>
        if (x.level < level + 1) {
          Right(())
        } else {
          Left(Err(
            s"[fulfill_type fail]\n" ++
              s"x: ${pretty_val(x)}\n" ++
              s"level: ${level}\n"))
        }
      case x: ValPi =>
        for {
          _ <- fulfill_type(x.arg_t, level)
          _ <- fulfill_type(x.dep_t.force(), level)
        } yield ()
      case x: ValFn =>
        for {
          _ <- fulfill_type(x.arg_t, level)
          _ <- fulfill_type(x.body.force(), level)
        } yield ()
      case x: ValClub =>
        fulfill_tel_type(x.tel, level)
      case x: ValMember =>
        fulfill_tel_type(x.tel, level)
      case x: ValRecord =>
        fulfill_tel_type(x.tel, level)
      case _ =>
        Left(Err(
          s"[fulfill_type fail]\n" ++
            s"x: ${pretty_val(x)}\n" ++
            s"level: ${level}\n"))
    }
  }

  def fulfill_tel_type(x: Tel, level: Int): Either[Err, Unit] = {
    val list = x.force().fields.map {
      case (_, _, _, Some(tv), _) => fulfill_type(tv, level)
      case (_, _, _, None, _) =>
        println(s"[internal error]")
        println(s"tel.force is not effective")
        throw new Exception()
    }
    first_err(list)
  }

}
