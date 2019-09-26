package xieyuheng.cicada

import readback._

import scala.annotation.tailrec

sealed trait Norm
final case class NormType(level: Int) extends Norm
final case class NormPi(arg_name: String, arg_t: Norm, dep_t: Norm) extends Norm
final case class NormFn(arg_name: String, arg_t: Norm, body: Norm) extends Norm
final case class NormClub(name: String, members: List[Member], norm_tel: NormTelescope) extends Norm
final case class NormMember(name: String, club_name: String, norm_tel: NormTelescope) extends Norm
final case class NormRecord(name: String, super_names: List[String], norm_tel: NormTelescope) extends Norm

case class NormTelescope(
  fields: List[(String, Exp, Option[Exp], Norm, Option[Norm])],
  seed: Seed,
  env: Env,
) {

  def put(arg: Norm): Either[Err, NormTelescope] = {
    val i = fields.indexWhere {
      case (_, _, _, _, None) => true
      case (_, _, _, _, Some(_)) => false
    }

    if (i == -1) {
      Left(Err(s"the norm telescope is full, fail to put: ${arg}"))
    } else {
      val (k, te, mve, tn, _) = fields(i)
      val new_fields = util.list_replace(fields, i,
        (k, te, mve, tn, Some(arg)))
      Right(
        NormTelescope(new_fields, seed, env)
          .self_put())
    }
  }

  def self_put(): NormTelescope = {
    val i = fields.indexWhere {
      case (_, _, _, _, None) => true
      case (_, _, _, _, Some(_)) => false
    }

    if (i == -1) {
      this
    } else {
      fields(i) match {
        case (k, te, Some(ve), tn, _) =>
          val arg = eval(ve, env)
          val new_fields = util.list_replace(fields, i,
            (k, te, Some(ve), tn, Some(readback_val(seed, arg))))
          NormTelescope(new_fields, seed, env.ext_val(k, arg))
        case _ => this
      }
    }
  }

}

sealed trait NormNeu extends Norm
final case class NormNeuVar(name: String, norm_arg_t: Norm) extends NormNeu
final case class NormNeuAp(target: NormNeu, arg: Norm) extends NormNeu
final case class NormNeuChoice(target: NormNeu, path: List[String], map: Map[String, Exp], seed: Seed, env: Env) extends NormNeu
final case class NormNeuDot(target: NormNeu, field_name: String) extends NormNeu
final case class NormNeuDotType(target: NormNeu, field_name: String) extends NormNeu
