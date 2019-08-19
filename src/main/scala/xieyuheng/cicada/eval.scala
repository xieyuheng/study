package xieyuheng.cicada

object eval {
  def apply(exp: Exp, env: Env): Either[ErrorMsg, Value] = {
    exp match {
      case Var(name) => {
        env.get(name) match {
          case Some(DefineValue(name, value)) =>
            Right(value)
          case Some(DefineRecord(name, map)) =>
            seqMap(map, env).flatMap { case (map, bind) =>
              Right(RecordValue(name, map, bind)) }
          case Some(DefineUnion(name, map, subNames)) =>
            seqMap(map, env).flatMap { case (map, bind) =>
              Right(UnionValue(util.newId(), name, map, subNames, bind)) }
          case None =>
            Right(NeutralValue(VarNeutral(name)))
        }
      }

      case Type() => {
        Right(LogicVar(util.newId()))
      }

      case Case(target, map) => {
        for {
          targetValue <- eval(target, env)
          result <- targetValue match {
            case record: RecordValue =>
              map.get(record.name) match {
                case Some(exp) => eval(exp, env)
                case None => Left(ErrorMsg(s"no clause: ${record.name}, on case: ${map}"))
              }
            case NeutralValue(neutral) =>
              for {
                map <- letMap(map, env)
              } yield NeutralValue(CaseNeutral(neutral, map))
            case _ =>
              Left(ErrorMsg("targetValue of Field should be RecordValue or NeutralValue, " +
                s"instead of: ${targetValue}"))
          }
        } yield result
      }

      case Field(target, fieldName) => {
        for {
          targetValue <- eval(target, env)
          result <- targetValue match {
            case union: UnionValue =>
              union.map.get(fieldName) match {
                case Some(value) => Right(util.deepWalk(value, union.bind))
                case None => Left(ErrorMsg(s"no field: ${fieldName}, on union: ${union}"))
              }
            case record: RecordValue =>
              record.map.get(fieldName) match {
                case Some(value) => Right(util.deepWalk(value, record.bind))
                case None => Left(ErrorMsg(s"no field: ${fieldName}, on record: ${record}"))
              }
            case NeutralValue(neutral) =>
              Right(NeutralValue(FieldNeutral(neutral, fieldName)))
            case _ =>
              Left(ErrorMsg("targetValue of Field should be UnionValue, RecordValue or NeutralValue, " +
                s"instead of: ${targetValue}"))
          }
        } yield result
      }

      case Pi(args, ret) => {
        for {
          args <- letMap(args, env)
          ret <- eval(ret, env)
        } yield PiValue(util.newId(), args, ret)
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
            exe(targetValue, argsValue)
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

    def extendEnv(valueMap: MultiMap[String, Value], env: Env): Env = {
      valueMap.entries.foldLeft(env) { case (env, (name, value)) =>
        env + (name -> DefineValue(name, value))
      }
    }

    def updateBind(
      valueMap: MultiMap[String, Value],
      bind: Bind,
      name: String,
      value: Value,
    ): Either[ErrorMsg, Bind] = {
      valueMap.get(name) match {
        case Some(oldValue) => fulfill(value, oldValue, bind)
        case None => Right(bind)
      }
    }

    map.entries.foldLeft(initResult) { case (result, (name, exp)) =>
      for {
        valueMapAndbind <- result
        (valueMap, bind) = valueMapAndbind
        value <- eval(exp, extendEnv(valueMap, env))
        newBind <- updateBind(valueMap, bind, name, value)
      } yield ((valueMap.update(name -> value), newBind))
    }
  }
}
