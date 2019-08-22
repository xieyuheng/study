package xieyuheng.cicada

import scala.collection.immutable.ListMap

object exe {
  def apply(
    target: Value,
    args: ListMap[String, Value],
    env: Env,
  ): Either[ErrorMsg, Value] = {
    target match {
      case sumType: SumTypeValue =>
        for {
          newBind <- unify.onMap(args, sumType.map, sumType.bind, env)
        } yield sumType.copy(bind = newBind)

      case memberType: MemberTypeValue =>
        for {
          newBind <- unify.onMap(args, memberType.map, memberType.bind, env)
        } yield memberType.copy(bind = newBind)

      case pi: PiValue =>
        Left(ErrorMsg(s"can not apply a PiValue: ${pi}"))

      case fn: FnValue =>
        for {
          bind <- unify.onMap(args, fn.args, Bind(), env)
          newArgs = walk.deepOnMap(fn.args, bind)
          value <- eval(fn.body, fn.env.extendByValueMap(newArgs))
          bind <- unify(value, fn.ret, bind, env)
          newValue = walk.deep(value, bind)
        } yield newValue

      case neu: NeutralValue =>
        Right(NeutralValue(ApNeutral(neu.neutral, args)))

      case v: Value =>
        Left(ErrorMsg(s"can not apply: ${v}"))
    }
  }
}
