package xieyuheng.cicada.with_logic_variable

import scala.collection.immutable.ListMap

object exe {
  def apply(
    target: Val,
    args: ListMap[String, Val],
    env: Env,
  ): Either[Err, Val] = {
    target match {
      case sumType: SumTypeVal =>
        for {
          newBind <- unify.onMap(args, sumType.map, sumType.bind, env)
        } yield sumType.copy(bind = newBind)

      case memberType: MemberTypeVal =>
        for {
          newBind <- unify.onMap(args, memberType.map, memberType.bind, env)
        } yield memberType.copy(bind = newBind)

      case pi: PiVal =>
        Left(Err(s"can not apply a PiVal: ${pi}"))

      case fn: FnVal =>
        for {
          bind <- unify.onMap(args, fn.args, Bind(), env)
          // TODO why the following is not right for `list_map_succ(exp: Exp)` ?
          // newArgs = walk.deepOnMap(fn.args, bind)
          // - it means unification is not effective here
          // - we need to test unification separately
          newArgs = walk.deepOnMap(args, bind)
          value <- eval(fn.body, fn.env.extendByValMap(newArgs))
          bind <- unify(value, fn.ret, bind, env)
        } yield walk.deep(value, bind)

      case neu: NeuVal =>
        Right(NeuVal(ApNeu(neu.neutral, args)))

      case v: Val =>
        Left(Err(s"can not apply: ${v}"))
    }
  }
}
