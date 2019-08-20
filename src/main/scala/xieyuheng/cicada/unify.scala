package xieyuheng.cicada

object unify {
  def apply(src: Value, tar: Value, bind: Bind, env: Env): Either[ErrorMsg, Bind] = {
    (util.walk(src, bind), util.walk(tar, bind)) match {
      case (src, tar) if {
        src == tar
      } => {
        Right(bind)
      }

      case (t1: TypeOfType, t2: TypeOfType) => {
        Right(bind + (t1.id -> t2))
      }

      case (sumType: SumTypeValue, t: TypeOfType) => {
        Right(bind + (t.id -> sumType))
      }

      case (t: TypeOfType, sumType: SumTypeValue) => {
        Right(bind + (t.id -> sumType))
      }

      case (memberType: MemberTypeValue, t: TypeOfType) => {
        for {
          sumType <- eval(Var(memberType.superName), env)
        } yield bind + (t.id -> sumType)
      }

      case (t: TypeOfType, memberType: MemberTypeValue) => {
        for {
          sumType <- eval(Var(memberType.superName), env)
        } yield bind + (t.id -> sumType)
      }

      case (pi: PiValue, t: TypeOfType) => {
        Right(bind + (t.id -> pi))
      }

      case (t: TypeOfType, pi: PiValue) => {
        Right(bind + (t.id -> pi))
      }

      case (fn: FnValue, pi: PiValue) => {
        for {
          /** contravariant at args */
          bind <- forMap(pi.args, fn.args, bind, env)
          bind <- unify(fn.ret, pi.ret, bind, env)
        } yield bind + (pi.id -> fn)
      }

      case (pi: PiValue, fn: FnValue) => {
        for {
          /** contravariant at args */
          bind <- forMap(pi.args, fn.args, bind, env)
          bind <- unify(fn.ret, pi.ret, bind, env)
        } yield bind + (pi.id -> fn)
      }

      case (memberType: MemberTypeValue, sumType: SumTypeValue) if {
        sumType.memberNames.contains(memberType.name)
      } => {
        for {
          bind <- forBind(memberType.bind, bind, env)
          bind <- forBind(sumType.bind, bind, env)
          bind <- forMap(memberType.map, sumType.map, bind, env)
        } yield bind + (sumType.id -> memberType)
      }

      case (sumType: SumTypeValue, memberType: MemberTypeValue) if {
        sumType.memberNames.contains(memberType.name)
      } => {
        for {
          bind <- forBind(memberType.bind, bind, env)
          bind <- forBind(sumType.bind, bind, env)
          bind <- forMap(memberType.map, sumType.map, bind, env)
        } yield bind + (sumType.id -> memberType)
      }

      case (src: SumTypeValue, tar: SumTypeValue) if {
        src.name == tar.name
      } => {
        for {
          bind <- forBind(src.bind, bind, env)
          bind <- forBind(tar.bind, bind, env)
          result <- forMap(src.map, tar.map, bind, env)
        } yield result
      }

      case (src: MemberTypeValue, tar: MemberTypeValue) if {
        src.name == tar.name
      } => {
        for {
          bind <- forBind(src.bind, bind, env)
          bind <- forBind(tar.bind, bind, env)
          result <- forMap(src.map, tar.map, bind, env)
        } yield result
      }

      case (src: PiValue, tar: PiValue) => {
        for {
          bind <- forMap(tar.args, src.args, bind, env)
          bind <- unify(src.ret, tar.ret, bind, env)
        } yield bind
      }

      case _ => {
        Left(ErrorMsg(
          "fail to unify\n" ++
            s"src: ${util.walk(src, bind)}\n" ++
            s"tar: ${util.walk(tar, bind)}\n" ++
            s"bind: ${bind}\n"))
      }
    }
  }

  def forMap(
    srcMap: MultiMap[String, Value],
    tarMap: MultiMap[String, Value],
    bind: Bind,
    env: Env,
  ): Either[ErrorMsg, Bind] = {
    val initResult: Either[ErrorMsg, Bind] =
      Right(bind)

    def updateBind(
      bind: Bind,
      name: String,
      tarValue: Value,
    ): Either[ErrorMsg, Bind] = {
      srcMap.get(name) match {
        case Some(srcValue) =>
          unify(srcValue, tarValue, bind, env)
        case None =>
          Left(ErrorMsg(s"srcMap does not have name: ${name}, tarValue: ${tarValue}"))
      }
    }

    tarMap.entries.foldLeft(initResult) { case (result, (name, tarValue)) =>
      result.flatMap { bind => updateBind(bind, name, tarValue) }
    }
  }

  def forBind(
    bind1: Bind,
    bind2: Bind,
    env: Env,
  ): Either[ErrorMsg, Bind] = {
    val initBind = bind1 ++ bind2.filterNot { case (id, _value) =>
      bind1.keys.toSet.contains(id) }

    assert(initBind.size >= bind1.size)

    val initResult: Either[ErrorMsg, Bind] =
      Right(initBind)

    def updateBind(bind: Bind, id: Id, v: Value): Either[ErrorMsg, Bind] = {
      bind.get(id) match {
        case Some(v1) =>
          unify(v1, v, bind, env)
        case None =>
          Left(ErrorMsg(s"forBind internal error, bind1: ${bind1}, bind2: ${bind2}"))
      }
    }

    bind2
      .filter { case (id, _value) => bind1.keys.toSet.contains(id) }
      .foldLeft(initResult) { case (result, (id, v)) =>
        result.flatMap { bind => updateBind(bind, id, v) }
      }
  }
}
