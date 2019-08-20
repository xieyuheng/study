package xieyuheng.cicada

object exe {
  def apply(
    target: Value,
    args: MultiMap[String, Value],
  ): Either[ErrorMsg, Value] = {
    target match {
      case t: TypeVar =>
        Left(ErrorMsg(s"can not apply a TypeVar: ${t}"))

      case union: UnionValue =>
        for {
          newBind <- unify.forMap(args, union.map, union.bind)
        } yield union.copy(bind = newBind)

      case record: RecordValue =>
        for {
          newBind <- unify.forMap(args, record.map, record.bind)
        } yield record.copy(bind = newBind)

      case pi: PiValue =>
        Left(ErrorMsg(s"can not apply a PiValue: ${pi}"))

      case fn: FnValue =>
        for {
          bind <- unify.forMap(args, fn.args, Bind())
          newArgs = util.deepWalkForMap(fn.args, bind)
          value <- eval(fn.body, fn.env.extendByValueMap(newArgs))
          bind <- unify(value, fn.ret, bind)
          newValue = util.deepWalk(value, bind)
        } yield newValue

      case neu: NeutralValue =>
        Right(NeutralValue(ApNeutral(neu.neutral, args)))
    }
  }
}
