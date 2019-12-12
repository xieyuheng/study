package xieyuheng.cicada

import java.util.UUID

import collection.immutable.ListMap

import eval._
import equivalent._

object subtype {

  def subtype(ctx: Ctx, s: Val, t: Val): Either[Err, Unit] = {
    (s, t) match {
      case (s: ValPi, t: ValPi) =>
        if (s.arg_type_map.keys.size != t.arg_type_map.keys.size) {
          Left(Err(s"subtype fail on ValPi, arity mis-match"))
        } else {
          val name_list = s.arg_type_map.keys.zip(t.arg_type_map.keys).map {
            case (s_name, t_name) =>
              val uuid: UUID = UUID.randomUUID()
              s"#subtype-pi-type:${s_name}:${t_name}:${uuid}"
          }.toList
          for {
            s_force <- force_pi(name_list, s)
            (s_arg_type_map, s_return_type) = s_force
            t_force <- force_pi(name_list, t)
            (t_arg_type_map, t_return_type) = t_force
            _ <- subtype_map(ctx, t_arg_type_map, s_arg_type_map)
            _ <- subtype(ctx, s_return_type, t_return_type)
          } yield ()
        }

      case (s: ValCl, t: ValCl) =>
        subtype_map(ctx, s.type_map, t.type_map)

      case (s, t) =>
        equivalent(ctx, s, t)
    }
  }

  def subtype_map(
    ctx: Ctx,
    s_map: ListMap[String, Val],
    t_map: ListMap[String, Val],
  ): Either[Err, Unit] = {
    util.list_map_foreach_maybe_err(t_map) {
      case (name, t) =>
        s_map.get(name) match {
          case Some(s) =>
            subtype(ctx, s, t)
          case None =>
            Left(Err(s"subtype_map can not find field: ${name}"))
        }
    }
  }

  def force_pi(
    name_list: List[String],
    pi: ValPi,
  ): Either[Err, (ListMap[String, Val], Val)] = {
    val full_var_map = ListMap(pi.arg_type_map.keys.toList.zip(name_list): _*)
    for {
      arg_type_map <- util.list_map_map_entry_with_index_maybe_err(pi.arg_type_map) {
        case (i, _name, exp) =>
          eval(pi.env, exp_subst_var_map(exp, full_var_map.take(i)))
            .map { case value => (name_list(i), value) }
      }
      return_type <- eval(pi.env, exp_subst_var_map(pi.return_type, full_var_map))
    } yield (arg_type_map, return_type)
  }

  def exp_subst_var_map(exp: Exp, var_map: ListMap[String, String]): Exp = {
    exp match {
      case Var(name: String) =>
        var_map.get(name) match {
          case Some(new_name) => Var(new_name)
          case None => exp
        }
      case Type() =>
        exp
      case Pi(arg_type_map: ListMap[String, Exp], return_type: Exp) =>
        val new_arg_type_map = ListMap(arg_type_map.map {
          case (name, exp) =>
            (name, exp_subst_var_map(exp, var_map))
        }.toList: _*)
        val new_return_type = exp_subst_var_map(return_type, var_map);
        Pi(arg_type_map, new_return_type)
      case Fn(arg_type_map: ListMap[String, Exp], body: Exp) =>
        val new_arg_type_map = ListMap(arg_type_map.map {
          case (name, exp) =>
            (name, exp_subst_var_map(exp, var_map))
        }.toList: _*)
        val new_body = exp_subst_var_map(body, var_map);
        Fn(arg_type_map, new_body)
      case Ap(target: Exp, arg_list: List[Exp]) =>
        val new_target = exp_subst_var_map(target, var_map)
        val new_arg_list = arg_list.map {
          case exp =>
            exp_subst_var_map(exp, var_map)
        }
        Ap(new_target, new_arg_list)
      case Cl(type_map: ListMap[String, Exp]) =>
        val new_type_map = ListMap(type_map.map {
          case (name, exp) =>
            (name, exp_subst_var_map(exp, var_map))
        }.toList: _*)
        Cl(new_type_map)
      case Obj(val_map: ListMap[String, Exp]) =>
        val new_val_map = ListMap(val_map.map {
          case (name, exp) =>
            (name, exp_subst_var_map(exp, var_map))
        }.toList: _*)
        Obj(new_val_map)
      case Dot(target: Exp, field: String) =>
        val new_target = exp_subst_var_map(target, var_map)
        Dot(new_target, field)
      case Block(let_map: ListMap[String, Exp], body: Exp) =>
        val new_let_map = ListMap(let_map.map {
          case (name, exp) =>
            (name, exp_subst_var_map(exp, var_map))
        }.toList: _*)
        val new_body = exp_subst_var_map(body, var_map)
        Block(new_let_map, new_body)
    }
  }

}
