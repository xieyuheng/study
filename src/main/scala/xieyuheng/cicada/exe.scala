package xieyuheng.cicada

object exe {
  def apply(
    target: Value,
    args: MultiMap[String, Value],
    env: Env,
  ): Either[ErrorMsg, Value] = {
    target match {
      case t: TypeVar =>
        Left(ErrorMsg(s"can not apply a TypeVar: ${t}"))

      case sumType: SumTypeValue =>
        for {
          newBind <- unify.forMap(args, sumType.map, sumType.bind, env)
        } yield sumType.copy(bind = newBind)

      case memberType: MemberTypeValue =>
        for {
          newBind <- unify.forMap(args, memberType.map, memberType.bind, env)
        } yield memberType.copy(bind = newBind)

      case pi: PiValue =>
        Left(ErrorMsg(s"can not apply a PiValue: ${pi}"))

      case fn: FnValue =>
        for {
          bind <- unify.forMap(args, fn.args, Bind(), env)
          newArgs = util.deepWalkForMap(fn.args, bind)
          value <- eval(fn.body, fn.env.extendByValueMap(newArgs))
          bind <- unify(value, fn.ret, bind, env)
          newValue = util.deepWalk(value, bind)
        } yield newValue

      case neu: NeutralValue =>
        Right(NeutralValue(ApNeutral(neu.neutral, args)))
    }
  }
}
