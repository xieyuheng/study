package xieyuheng.tartlet

trait Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp]
}

case object ValAbsurd extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] =
    Right(Absurd)
}

case object ValAtom extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] =
    Right(Atom)
}

case class ValQuote (sym: String) extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] =
    Right(Quote(sym))
}

case class TheNeu(t: Val, neutral: Neu) extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] = {
    t match {
      case ValAbsurd =>
        for {
          normal <- neutral.readback_neu(ctx)
        } yield The(Absurd, normal)
      case _ =>
        neutral.readback_neu(ctx)
    }
  }
}

case class TheVal(t: Val, value: Val) {
  def readback_the_val(ctx: Ctx): Either[Err, Exp] = {
    value.readback_val(ctx, t)
  }
}


case class ValEqv (
  t: Val,
  from: Val,
  to: Val,
) extends Val {
  def readback_val(ctx: Ctx, _t: Val): Either[Err, Exp] = {
    for {
      t <- this.t.readback_val(ctx, ValUniverse)
      from <- from.readback_val(ctx, this.t)
      to <- to.readback_val(ctx, this.t)
    } yield Eqv(t, from, to)
  }
}

case object ValSame extends Val {
  def readback_val(ctx: Ctx, _t: Val): Either[Err, Exp] = {
    Right(Same)
  }
}

case class ValAdd1 (prev: Val) extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] =
    for {
      prev_exp <- prev.readback_val(ctx, t)
    } yield Add1(prev_exp)
}

case object ValNat extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] =
    Right(Nat)
}


case object ValZero extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] =
    Right(Zero)
}

case class ValFn(clo: Clo) extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] =
    t match {
      case ValPi(arg_t, ret_t) => {
        val fresh_name = util.freshen(ctx.names, ret_t.name)
        val arg = TheNeu(arg_t, NeuVar(fresh_name))
        for {
          bodyVal <- Ap.exe(this, arg)
          realRetType <- ret_t.apply(arg)
          body <- bodyVal.readback_val(
            ctx.ext(fresh_name, Bind(arg_t)),
            realRetType)
        } yield Fn(fresh_name, body)
      }
      case _ =>
        Left(Err(s"type of Fn should be Pi: ${t}"))
    }
}


case class ValPi (
  arg_t: Val,
  ret_t: Clo,
) extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] = {
    val fresh_name = util.freshen(ctx.names, ret_t.name)
    for {
      arg_tExp <- arg_t.readback_val(ctx, ValUniverse)
      ret_tExpVal <- ret_t.apply(
        TheNeu(arg_t, NeuVar(fresh_name)))
      ret_tExp <- ret_tExpVal.readback_val(
        ctx.ext(fresh_name, Bind(arg_t)), ValUniverse)
    } yield Pi(fresh_name, arg_tExp, ret_tExp)
  }
}

case class ValCons (car: Val, cdr: Val) extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] =
    for {
      car <- car.readback_val(ctx, t)
      cdr <- cdr.readback_val(ctx, t)
    } yield Cons(car, cdr)
}


case class ValSigma (
  arg_t: Val,
  cdr_t: Clo,
) extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] = {
    val fresh_name = util.freshen(ctx.names, cdr_t.name)
    for {
      arg_tExp <- arg_t.readback_val(ctx, ValUniverse)
      cdr_tExpVal <- cdr_t.apply(
        TheNeu(arg_t, NeuVar(fresh_name)))
      cdr_tExp <- cdr_tExpVal.readback_val(
        ctx.ext(fresh_name, Bind(arg_t)), ValUniverse)
    } yield Sigma(fresh_name, arg_tExp, cdr_tExp)
  }
}

case object ValSole extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] =
    Right(Sole)
}


case object ValTrivial extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] =
    Right(Trivial)
}

case object ValUniverse extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] =
    Right(Universe)
}


trait Neu {
  def readback_neu (ctx: Ctx): Either[Err, Exp]
}

case class NeuVar (
  name: String,
) extends Neu {
  def readback_neu(ctx: Ctx): Either[Err, Exp] = {
    Right(Var(name))
  }
}

case class NeuReplace (
  target: Neu,
  motive: TheVal,
  base: TheVal,
) extends Neu {
  def readback_neu(ctx: Ctx): Either[Err, Exp] = {
    for {
      target <- target.readback_neu(ctx)
      motive <- motive.readback_the_val(ctx)
      base <- base.readback_the_val(ctx)
    } yield Replace(target, motive, base)
  }
}

case class NeuNatInd (
  target: Neu,
  motive: TheVal,
  base: TheVal,
  step: TheVal,
) extends Neu {
  def readback_neu(ctx: Ctx): Either[Err, Exp] = {
    for {
      target <- target.readback_neu(ctx)
      motive <- motive.readback_the_val(ctx)
      base <- base.readback_the_val(ctx)
      step <- step.readback_the_val(ctx)
    } yield NatInd(target, motive, base, step)
  }
}

case class NeuAbsurdInd (
  target: Neu,
  motive: TheVal,
) extends Neu {
  def readback_neu (ctx: Ctx): Either[Err, Exp] = {
    for {
      target <- target.readback_neu(ctx)
      motive <- motive.readback_the_val(ctx)
    } yield AbsurdInd(The(Absurd, target), motive)
  }
}

case class NeuAp (
  fn: Neu,
  arg: TheVal,
) extends Neu {
  def readback_neu (ctx: Ctx): Either[Err, Exp] = {
    for {
      rator <- fn.readback_neu(ctx)
      rand <- arg.readback_the_val(ctx)
    } yield Ap(rator, rand)
  }
}

case class NeuCar (
  pair: Neu,
) extends Neu {
  def readback_neu(ctx: Ctx): Either[Err, Exp] = {
    for {
      pair <- pair.readback_neu(ctx)
    } yield Car(pair)
  }
}

case class NeuCdr (
  pair: Neu,
) extends Neu {
  def readback_neu(ctx: Ctx): Either[Err, Exp] = {
    for {
      pair <- pair.readback_neu(ctx)
    } yield Cdr(pair)
  }
}

sealed trait Clo {
  def name: String
  def apply(value: Val): Either[Err, Val]
}

final case class NativeClo(name: String, fn: Val => Either[Err, Val]) extends Clo {
  def apply(value: Val): Either[Err, Val] =
    fn(value)
}

final case class EnvClo(env: Env, name: String, body: Exp) extends Clo {
  def apply(value: Val): Either[Err, Val] =
    eval(body, env.ext (name, value))
}
