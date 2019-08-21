package xieyuheng.cicada

object eval {
  def apply(exp: Exp, env: Env): Either[ErrorMsg, Value] = {
    exp match {
      case Var(name) => {
        env.get(name) match {
          case Some(DefineValue(name, value)) =>
            Right(value)
          case Some(DefineMemberType(name, map, superName)) =>
            seqMap(map, env).flatMap { case (map, bind) =>
              Right(MemberTypeValue(name, map, superName, bind)) }
          case Some(DefineSumType(name, map, memberNames)) =>
            seqMap(map, env).flatMap { case (map, bind) =>
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

      case OfType(t) => {
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
                map <- letMap(map, env)
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
                case Some(value) => Right(util.deepWalk(value, sumType.bind))
                case None => Left(ErrorMsg(s"no field: ${fieldName}, on sumType: ${sumType}"))
              }
            case memberType: MemberTypeValue =>
              memberType.map.get(fieldName) match {
                case Some(value) => Right(util.deepWalk(value, memberType.bind))
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
          args <- letMap(args, env)
          ret <- eval(ret, env)
        } yield PiValue(args, ret)
      }

      case Fn(args, ret, body) => {
        for {
          args <- letMap(args, env)
          ret <- eval(ret, env)
        } yield FnValue(args, ret, body, env)
      }

      case Ap(target, args) => {
        eval(target, env).flatMap { targetValue =>
          letMap(args, env).flatMap { argsValue =>
            exe(targetValue, argsValue, env)
          }
        }
      }
    }
  }

  /** like scheme (let) */
  def letMap(
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
      eval(exp, env).flatMap { value =>
        Right(map.update(name -> value))
      }
    }

    map.entries.foldLeft(initResult) { case (result, (name, exp)) =>
      result.flatMap { valueMap => updateValueMap(valueMap, name, exp) }
    }
  }

  /** like scheme (let*) */
  def seqMap(
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
