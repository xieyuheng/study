package xieyuheng.minitt

sealed trait Ctx {
  def lookup(name: String): Option[Val] = {
    this match {
      case CtxVar(name2: String, t: Val, rest: Ctx) =>
        if (name2 == name) {
          Some(t)
        } else {
          rest.lookup(name)
        }
      case CtxEmpty() => None
    }
  }

  def ext(pat: Pat, t: Val, v: Val): Either[Err, Ctx] = {
    pat match {
      case PatVar(name: String) =>
        Right(CtxVar(name, t, this))
      case PatCons(car_pat: Pat, cdr_pat: Pat) =>
        (t, v) match {
          case (ValSigma(arg_t: Val, clo_fn: CloFn), ValCons(car: Val, cdr: Val)) =>
            for {
              ctx1 <- this.ext(car_pat, arg_t, car)
              ctx2 <- ctx1.ext(cdr_pat, clo_fn.ap(car), cdr)
            } yield ctx2
          case _ =>
            Left(Err(
              s"fail to extend ctx: ${this}\n" ++
                s"pat: ${pat}\n" ++
                s"t: ${t}\n" ++
                s"v: ${v}\n"))
        }
      case PatSole() =>
        Right(this)
    }
  }
}

final case class CtxVar(name: String, t: Val, rest: Ctx) extends Ctx
final case class CtxEmpty() extends Ctx
