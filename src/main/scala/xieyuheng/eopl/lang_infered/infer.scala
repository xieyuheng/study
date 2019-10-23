package xieyuheng.eopl.lang_infered

import pretty._

import xieyuheng.util.err._

object infer {

  def subst_type_var(t: Type, serial: Int, arg_t: Type): Type = {
    t match {
      case TypeVar(serial2: Int, aka: Option[String]) =>
        if (serial2 == serial) {
          arg_t
        } else {
          t
        }
      case TypeInt() =>
        TypeInt()
      case TypeBool() =>
        TypeBool()
      case TypeSole() =>
        TypeSole()
      case TypeArrow(arg_t2: Type, ret_t: Type) =>
        TypeArrow(
          subst_type_var(arg_t2, serial, arg_t),
          subst_type_var(ret_t, serial, arg_t))
    }
  }

  def subst_bind(t: Type, bind: Bind): Type = {
    t match {
      case TypeVar(serial: Int, aka: Option[String]) =>
        bind.lookup_type(serial) match {
          case Some(arg_t) =>
            arg_t
          case None =>
            t
        }
      case TypeInt() =>
        TypeInt()
      case TypeBool() =>
        TypeBool()
      case TypeSole() =>
        TypeSole()
      case TypeArrow(arg_t: Type, ret_t: Type) =>
        TypeArrow(
          subst_bind(arg_t, bind),
          subst_bind(ret_t, bind))
    }
  }

  def subst_bind_ext(bind: Bind, serial: Int, arg_t: Type): Bind = {
    val new_bind = bind.map {
      case (n, t) =>
        (n, subst_type_var(t, serial, arg_t))
    }
    new_bind.ext(serial, arg_t)
  }

  // NOTE
  //   subst_bind(t, subst_bind_ext(bind, serial, arg_t)) ===
  //   subst_type_var(subst_bind(t, bind), serial, arg_t)

  // NOTE
  //   the above implementation preserves the no-occurrence invariant,
  //   but it does not depend on, nor does it attempt to enforce it.
  //   (occurrence checking is the job of the unifier.)

  def unify(bind: Bind, x: Type, y: Type, cause: Exp): Either[Err, Bind] = {
    (subst_bind(x, bind), subst_bind(y, bind)) match {
      case (x, y) if x == y =>
        Right(bind)
      case (TypeVar(serial, aka), y) =>
        if (occur_p(serial, y)) {
          Left(Err(
            s"[unify fail]\n" ++
              s"type variable on left hand side occurs in right hand side\n" ++
              s"type variable: ${pretty_type(TypeVar(serial, aka))}\n" ++
              s"right hand side: ${pretty_type(y)}\n" ++
              s"equation of exp: ${pretty_exp(cause)}\n"
          ))
        } else {
          Right(subst_bind_ext(bind, serial, y))
        }
      case (x, TypeVar(serial, aka)) =>
        if (occur_p(serial, x)) {
          Left(Err(
            s"[unify fail]\n" ++
              s"type variable on right hand side occurs in left hand side\n" ++
              s"type variable: ${pretty_type(TypeVar(serial, aka))}\n" ++
              s"left hand side: ${pretty_type(x)}\n" ++
              s"equation of exp: ${pretty_exp(cause)}\n"
          ))
        } else {
          Right(subst_bind_ext(bind, serial, x))
        }
      case (x: TypeArrow, y: TypeArrow) =>
        unify(bind, x.arg_t, y.arg_t, cause).flatMap {
          case bind2 =>
            unify(bind, x.ret_t, y.ret_t, cause)
        }
      case (x, y) =>
        Left(Err(
          s"[unify fail]\n" ++
            s"left hand side: ${pretty_type(x)}\n" ++
            s"right hand side: ${pretty_type(y)}\n" ++
            s"equation of exp: ${pretty_exp(cause)}\n"
        ))
    }
  }

  def occur_p(serial: Int, t: Type): Boolean = {
    t match {
      case TypeVar(serial2: Int, aka: Option[String]) =>
        serial2 == serial
      case TypeInt() =>
        false
      case TypeBool() =>
        false
      case TypeSole() =>
        false
      case TypeArrow(arg_t: Type, ret_t: Type) =>
        occur_p(serial, arg_t) || occur_p(serial, ret_t)
    }
  }

  def type_eq(map: Map[Int, Int], x: Type, y: Type): Boolean = {
    ???
  }

  // def exp_equation(exp: Exp): = {}

  def infer(seed: Int, bind: Bind, ctx: Ctx, exp: Exp): Either[Err, (Bind, Type)] = {
    ???
  }

}
