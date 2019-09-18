package xieyuheng.cicada.with_logic_variable

import scala.collection.immutable.ListMap

object eval {
  def apply(exp: Exp, env: Env): Either[Err, Val] = {
    exp match {
      case Var(name) => {
        env.get(name) match {
          case Some(DefineVal(name, value)) =>
            Right(value)
          case Some(DefineMemberType(name, map, superName)) =>
            eval.yieldBind(map, env).flatMap { case (map, bind) =>
              Right(MemberTypeVal(name, map, superName, bind)) }
          case Some(DefineSumType(name, map, memberNames)) =>
            eval.yieldBind(map, env).flatMap { case (map, bind) =>
              Right(SumTypeVal(name, map, memberNames, bind)) }
          case Some(DefineFn(name, args, ret, body)) =>
            eval(Fn(args, ret, body), env)
          case None =>
            Right(NeuVal(VarNeu(name)))
        }
      }

      case Type() => {
        Right(TypeOfType(Id()))
      }

      case The(t) => {
        for {
          t <- eval(t, env)
        } yield ValOfType(Id(), t)
      }

      case Choice(target, map) => {
        for {
          targetVal <- eval(target, env)
          result <- targetVal match {
            case memberType: MemberTypeVal =>
              map.get(memberType.name) match {
                case Some(exp) => eval(exp, env)
                case None => Left(Err(s"no clause: ${memberType.name}, on case: ${map}"))
              }
            case NeuVal(neutral) =>
              for {
                map <- eval.onMap(map, env)
              } yield NeuVal(ChoiceNeu(neutral, map))
            case _ =>
              Left(Err("targetVal of Dot should be MemberTypeVal or NeuVal, " +
                s"instead of: ${targetVal}"))
          }
        } yield result
      }

      case Dot(target, field_name) => {
        for {
          targetVal <- eval(target, env)
          result <- targetVal match {
            case sumType: SumTypeVal =>
              sumType.map.get(field_name) match {
                case Some(value) => Right(walk.deep(value, sumType.bind))
                case None => Left(Err(s"no field: ${field_name}, on sumType: ${sumType}"))
              }
            case memberType: MemberTypeVal =>
              memberType.map.get(field_name) match {
                case Some(value) => Right(walk.deep(value, memberType.bind))
                case None => Left(Err(s"no field: ${field_name}, on memberType: ${memberType}"))
              }
            case NeuVal(neutral) =>
              Right(NeuVal(DotNeu(neutral, field_name)))
            case _ =>
              Left(Err("targetVal of Dot should be SumTypeVal, MemberTypeVal or NeuVal, " +
                s"instead of: ${targetVal}"))
          }
        } yield result
      }

      case Pi(args, ret) => {
        for {
          (args, env) <- eval.yieldEnv(args, env)
          ret <- eval(ret, env)
        } yield PiVal(args, ret)
      }

      case Fn(args, ret, body) => {
        for {
          (args, env) <- eval.yieldEnv(args, env)
          ret <- eval(ret, env)
        } yield FnVal(args, ret, body, env)
      }

      case Ap(target, args) => {
        for {
          targetVal <- eval(target, env)
          argsVal <- eval.onMap(args, env)
          value <- exe(targetVal, argsVal, env)
        } yield value
      }
    }
  }

  def onMap(
    map: MultiMap[String, Exp],
    env: Env,
  ): Either[Err, ListMap[String, Val]] = {
    val initResult: Either[Err, ListMap[String, Val]] =
      Right(ListMap())

    def updateValMap(
      map: ListMap[String, Val],
      name: String,
      exp: Exp,
    ): Either[Err, ListMap[String, Val]] = {
      for {
        value <- eval(exp, env)
      } yield map + (name -> value)
    }

    map.entries.foldLeft(initResult) { case (result, (name, exp)) =>
      result.flatMap { case (valueMap) => updateValMap(valueMap, name, exp) }
    }
  }

  /**
    * to use pattern match on Either[Err, A]
    * in for block
    */
  implicit class EitherWithFilter[A](self: Either[Err, A]) {
    def withFilter(p: A => Boolean): Either[Err, A] =
      self match {
        case Right(value) => {
          if (p(value)) {
            Right(value)
          } else {
            Left(Err(s"filtered out: ${value}, by ${p}"))
          }
        }
        case Left(error) =>
          Left(error)
      }
  }

  def yieldEnv(
    map: MultiMap[String, Exp],
    env: Env,
  ): Either[Err, (ListMap[String, Val], Env)] = {
    val initResult: Either[Err, (ListMap[String, Val], Env)] =
      Right(ListMap(), env)

    def updateValMap(
      map: ListMap[String, Val],
      name: String,
      exp: Exp,
      env: Env,
    ): Either[Err, (ListMap[String, Val], Env)] = {
      for {
        value <- eval(exp, env)
      } yield (map + (name -> value), env.extend(name -> DefineVal(name, value)))
    }

    map.entries.foldLeft(initResult) { case (result, (name, exp)) =>
      result.flatMap { case (valueMap, env) => updateValMap(valueMap, name, exp, env) }
    }
  }

  def yieldBind(
    map: MultiMap[String, Exp],
    env: Env,
  ): Either[Err, (ListMap[String, Val], Bind)] = {
    val initResult: Either[Err, (ListMap[String, Val], Bind)] =
      Right((ListMap(), Bind()))

    def updateBind(
      valueMap: ListMap[String, Val],
      bind: Bind,
      name: String,
      value: Val,
    ): Either[Err, Bind] = {
      valueMap.get(name) match {
        case Some(oldVal) => unify(value, oldVal, bind, env)
        case None => Right(bind)
      }
    }

    map.entries.foldLeft(initResult) { case (result, (name, exp)) =>
      for {
        valueMapAndbind <- result
        (valueMap, bind) = valueMapAndbind
        value <- eval(exp, env.extendByValMap(valueMap))
        newBind <- updateBind(valueMap, bind, name, value)
      } yield ((valueMap + (name -> value), newBind))
    }
  }
}
