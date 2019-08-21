package xieyuheng.cicada

object eval {
  def apply(exp: Exp, env: Env): Either[ErrorMsg, Value] = {
    exp match {
      case Var(name) => {
        env.get(name) match {
          case Some(DefineValue(name, value)) =>
            Right(value)
          case Some(DefineMemberType(name, map, superName)) =>
            eval.yieldBind(map, env).flatMap { case (map, bind) =>
              Right(MemberTypeValue(name, map, superName, bind)) }
          case Some(DefineSumType(name, map, memberNames)) =>
            eval.yieldBind(map, env).flatMap { case (map, bind) =>
              Right(SumTypeValue(name, map, memberNames, bind)) }
          case Some(DefineFn(name, args, ret, body)) =>
            eval(Fn(args, ret, body), env)
          case None =>
            Right(NeutralValue(VarNeutral(name)))
        }
      }

      case Type() => {
        Right(TypeOfType(Id()))
      }

      case The(t) => {
        for {
          t <- eval(t, env)
        } yield ValueOfType(Id(), t)
      }

      case Case(target, map) => {
        for {
          targetValue <- eval(target, env)
          result <- targetValue match {
            case memberType: MemberTypeValue =>
              map.get(memberType.name) match {
                case Some(exp) => eval(exp, env)
                case None => Left(ErrorMsg(s"no clause: ${memberType.name}, on case: ${map}"))
              }
            case NeutralValue(neutral) =>
              for {
                map <- eval.onMap(map, env)
              } yield NeutralValue(CaseNeutral(neutral, map))
            case _ =>
              Left(ErrorMsg("targetValue of Field should be MemberTypeValue or NeutralValue, " +
                s"instead of: ${targetValue}"))
          }
        } yield result
      }

      case Field(target, fieldName) => {
        for {
          targetValue <- eval(target, env)
          result <- targetValue match {
            case sumType: SumTypeValue =>
              sumType.map.get(fieldName) match {
                case Some(value) => Right(walk.deep(value, sumType.bind))
                case None => Left(ErrorMsg(s"no field: ${fieldName}, on sumType: ${sumType}"))
              }
            case memberType: MemberTypeValue =>
              memberType.map.get(fieldName) match {
                case Some(value) => Right(walk.deep(value, memberType.bind))
                case None => Left(ErrorMsg(s"no field: ${fieldName}, on memberType: ${memberType}"))
              }
            case NeutralValue(neutral) =>
              Right(NeutralValue(FieldNeutral(neutral, fieldName)))
            case _ =>
              Left(ErrorMsg("targetValue of Field should be SumTypeValue, MemberTypeValue or NeutralValue, " +
                s"instead of: ${targetValue}"))
          }
        } yield result
      }

      case Pi(args, ret) => {
        for {
          (args, env) <- eval.yieldEnv(args, env)
          ret <- eval(ret, env)
        } yield PiValue(args, ret)
      }

      case Fn(args, ret, body) => {
        for {
          (args, env) <- eval.yieldEnv(args, env)
          ret <- eval(ret, env)
        } yield FnValue(args, ret, body, env)
      }

      case Ap(target, args) => {
        for {
          targetValue <- eval(target, env)
          argsValue <- eval.onMap(args, env)
          value <- exe(targetValue, argsValue, env)
        } yield value
      }
    }
  }

  def onMap(
    map: MultiMap[String, Exp],
    env: Env,
  ): Either[ErrorMsg, MultiMap[String, Value]] = {
    val initResult: Either[ErrorMsg, MultiMap[String, Value]] =
      Right(MultiMap())

    def updateValueMap(
      map: MultiMap[String, Value],
      name: String,
      exp: Exp,
    ): Either[ErrorMsg, MultiMap[String, Value]] = {
      for {
        value <- eval(exp, env)
      } yield map.update(name -> value)
    }

    map.entries.foldLeft(initResult) { case (result, (name, exp)) =>
      result.flatMap { case (valueMap) => updateValueMap(valueMap, name, exp) }
    }
  }

  /**
    * to use pattern match on Either[ErrorMsg, A]
    * in for block
    */
  implicit class EitherWithFilter[A](self: Either[ErrorMsg, A]) {
    def withFilter(p: A => Boolean): Either[ErrorMsg, A] =
      self match {
        case Right(value) => {
          if (p(value)) {
            Right(value)
          } else {
            Left(ErrorMsg(s"filtered out: ${value}, by ${p}"))
          }
        }
        case Left(error) =>
          Left(error)
      }
  }

  def yieldEnv(
    map: MultiMap[String, Exp],
    env: Env,
  ): Either[ErrorMsg, (MultiMap[String, Value], Env)] = {
    val initResult: Either[ErrorMsg, (MultiMap[String, Value], Env)] =
      Right(MultiMap(), env)

    def updateValueMap(
      map: MultiMap[String, Value],
      name: String,
      exp: Exp,
      env: Env,
    ): Either[ErrorMsg, (MultiMap[String, Value], Env)] = {
      for {
        value <- eval(exp, env)
      } yield (map.update(name -> value), env.defValue(name, value))
    }

    map.entries.foldLeft(initResult) { case (result, (name, exp)) =>
      result.flatMap { case (valueMap, env) => updateValueMap(valueMap, name, exp, env) }
    }
  }

  def yieldBind(
    map: MultiMap[String, Exp],
    env: Env,
  ): Either[ErrorMsg, (MultiMap[String, Value], Bind)] = {
    val initResult: Either[ErrorMsg, (MultiMap[String, Value], Bind)] =
      Right((MultiMap(), Bind()))

    def updateBind(
      valueMap: MultiMap[String, Value],
      bind: Bind,
      name: String,
      value: Value,
    ): Either[ErrorMsg, Bind] = {
      valueMap.get(name) match {
        case Some(oldValue) => unify(value, oldValue, bind, env)
        case None => Right(bind)
      }
    }

    map.entries.foldLeft(initResult) { case (result, (name, exp)) =>
      for {
        valueMapAndbind <- result
        (valueMap, bind) = valueMapAndbind
        value <- eval(exp, env.extendByValueMap(valueMap))
        newBind <- updateBind(valueMap, bind, name, value)
      } yield ((valueMap.update(name -> value), newBind))
    }
  }
}
