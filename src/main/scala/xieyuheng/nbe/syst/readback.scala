package xieyuheng.syst

import xieyuheng.util.err._

object readback {

  def readback_val(value: Val, used_names: Set[String], t: Type): Either[Err, Exp] = {
    value match {
      case TheNeu(t: Type, neu: Neu) =>
        readback_neu(neu, used_names)
      case ValSucc(prev: Val) =>
        t match {
          case Nat() =>
            for {
              prev_exp <- readback_val(prev, used_names, t)
            } yield Succ(prev_exp)
          case _ =>
            Left(Err(
              s"type of ValSucc should be Nat: ${t}"))
        }
      case ValZero() =>
        t match {
          case Nat() =>
            Right(Zero())
          case _ =>
            Left(Err(
              s"type of ValZero() should be Nat: ${t}"))
        }
      case ValFn(name: String, body: Exp, env: Env) =>
        t match {
          case Arrow(arg_t, dep_t) =>
            val fresh_name = freshen (used_names, name)
            val value2 = eval.exe_ap(value, TheNeu(arg_t, NeuVar(fresh_name)))
            for {
              body2 <- readback_val(value2, used_names + fresh_name, dep_t)
            } yield Fn(fresh_name, body2)
          case _ =>
            Left(Err(
              s"type of lambda should be arrow: ${t}"))
        }
    }
  }

  def readback_neu(neu: Neu, used_names: Set[String]): Either[Err, Exp] = {
    neu match {
      case NeuNatRec(t: Type, target: Neu, base: TheVal, step: TheVal) =>
        for {
          targetExp <- readback_neu(target, used_names)
          baseExp <- readback_the_val(base, used_names)
          stepExp <- readback_the_val(step, used_names)
        } yield NatRec(t, targetExp, baseExp, stepExp)
      case NeuAp(fn: Neu, arg: TheVal) =>
        for {
          rator <- readback_neu(fn, used_names)
          arg <- readback_the_val(arg, used_names)
        } yield Ap(rator, arg)
      case NeuVar(name: String) => {
        Right(Var(name))
      }
    }
  }

  def readback_the_val(the: TheVal, used_names: Set [String]): Either[Err, Exp] = {
    readback_val(the.value, used_names, the.t)
  }
}
