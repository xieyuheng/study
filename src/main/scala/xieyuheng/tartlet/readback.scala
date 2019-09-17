package xieyuheng.tartlet

object readback {

  def readback_val(value: Val, t: Val, ctx: Ctx): Either[Err, Exp] = {
    value match {
      case ValAbsurd() => Right(Absurd())
      case ValAtom() => Right(Atom())
      case ValQuote(sym: String) => Right(Quote(sym))
      case TheNeu(t: Val, neu: Neu) =>
        t match {
          case ValAbsurd() =>
            for {
              normal <- readback_neu(neu, ctx)
            } yield The(Absurd(), normal)
          case _ =>
            readback_neu(neu, ctx)
        }
      case ValEqv(t: Val, from: Val, to: Val) =>
        for {
          t_exp <- readback_val(t, ValUniverse(), ctx)
          from <- readback_val(from, t, ctx)
          to <- readback_val(to, t, ctx)
        } yield Eqv(t_exp, from, to)
      case ValSame() => Right(Same())
      case ValSucc(prev: Val) =>
        for {
          prev_exp <- readback_val(prev, t, ctx)
        } yield Succ(prev_exp)
      case ValNat() => Right(Nat())
      case ValZero() => Right(Zero())
      case ValFn(clo: Clo) =>
        t match {
          case ValPi(arg_t, ret_t) => {
            val fresh_name = freshen(ctx.names, ret_t.name)
            val arg = TheNeu(arg_t, NeuVar(fresh_name))
            for {
              body_val <- Ap.exe(value, arg)
              real_ret_t <- ret_t.ap(arg)
              body <- readback_val(
                body_val,
                real_ret_t,
                ctx.ext(fresh_name, Bind(arg_t)))
            } yield Fn(fresh_name, body)
          }
          case _ =>
            Left(Err(s"type of Fn should be Pi: ${t}"))
        }
      case ValPi(arg_t: Val, ret_t: Clo) =>
        val fresh_name = freshen(ctx.names, ret_t.name)
        for {
          arg_t_exp <- readback_val(arg_t, ValUniverse(), ctx)
          ret_t_exp_val <- ret_t.ap(
            TheNeu(arg_t, NeuVar(fresh_name)))
          ret_t_exp <- readback_val(
            ret_t_exp_val,
            ValUniverse(),
            ctx.ext(fresh_name, Bind(arg_t)))
        } yield Pi(fresh_name, arg_t_exp, ret_t_exp)
      case ValCons(car: Val, cdr: Val) =>
        for {
          car <- readback_val(car, t, ctx)
          cdr <- readback_val(cdr, t, ctx)
        } yield Cons(car, cdr)
      case ValSigma(arg_t: Val, cdr_t: Clo) =>
        val fresh_name = freshen(ctx.names, cdr_t.name)
        for {
          arg_t_exp <- readback_val(arg_t, ValUniverse(), ctx)
          cdr_t_exp_val <- cdr_t.ap(
            TheNeu(arg_t, NeuVar(fresh_name)))
          cdr_t_exp <- readback_val(
            cdr_t_exp_val,
            ValUniverse(),
            ctx.ext(fresh_name, Bind(arg_t)))
        } yield Sigma(fresh_name, arg_t_exp, cdr_t_exp)
      case ValSole() => Right(Sole())
      case ValTrivial() => Right(Trivial())
      case ValUniverse() => Right(Universe())
    }
  }

  def readback_neu(neu: Neu, ctx: Ctx): Either[Err, Exp] = {
    neu match {
      case NeuVar(name: String) => Right(Var(name))
      case NeuReplace(target: Neu, motive: TheVal, base: TheVal) =>
        for {
          target <- readback_neu(target, ctx)
          motive <- readback_the_val(motive, ctx)
          base <- readback_the_val(base, ctx)
        } yield Replace(target, motive, base)
      case NeuNatInd(target: Neu, motive: TheVal, base: TheVal, step: TheVal) =>
        for {
          target <- readback_neu(target, ctx)
          motive <- readback_the_val(motive, ctx)
          base <- readback_the_val(base, ctx)
          step <- readback_the_val(step, ctx)
        } yield NatInd(target, motive, base, step)
      case NeuAbsurdInd(target: Neu, motive: TheVal) =>
        for {
          target <- readback_neu(target, ctx)
          motive <- readback_the_val(motive, ctx)
        } yield AbsurdInd(The(Absurd(), target), motive)
      case NeuAp(fn: Neu, arg: TheVal) =>
        for {
          rator <- readback_neu(fn, ctx)
          rand <- readback_the_val(arg, ctx)
        } yield Ap(rator, rand)
      case NeuCar(pair: Neu) =>
        for {
          pair <- readback_neu(pair, ctx)
        } yield Car(pair)
      case NeuCdr(pair: Neu) =>
        for {
          pair <- readback_neu(pair, ctx)
        } yield Cdr(pair)
    }
  }

  def readback_the_val(the: TheVal, ctx: Ctx): Either[Err, Exp] = {
    readback_val(the.value, the.t, ctx)
  }

}
