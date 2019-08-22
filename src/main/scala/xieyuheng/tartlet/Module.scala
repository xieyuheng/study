package xieyuheng..tartlet

case class Module(
  var ctx: Ctx = Ctx(),
) {
  def claim(name: String, t: Exp): Module = {
    if (ctx.lookupType(name).isDefined) {
      println(s"name: ${name} is alreay claimed")
    } else {
      t.check(ctx, ValueUniverse) match {
        case Right(checkedTypeExp) =>
          for {
            checkedType <- checkedTypeExp.eval(ctx.toEnv)
          } ctx = ctx.ext(name, Bind(checkedType))
        case Left(errorMsg) =>
          println(s"type check fail, name: ${name}, errorMsg: ${errorMsg}")
      }
    }
    this
  }

  def define(name: String, exp: Exp): Module = {
    ctx.lookupDen(name) match {
      case Some(Bind(typeValue)) =>
        exp.check(ctx, typeValue) match {
          case Right(exp) =>
            for {
              value <- exp.eval(ctx.toEnv)
            } ctx = ctx.ext(name, Def(typeValue, value))
          case Left(errorMsg) =>
            println(s"type check fail for name: ${name}, errorMsg: ${errorMsg}")
        }
      case Some(Def(typeValue, value)) =>
        println(s"name: ${name} is already defined, type: ${typeValue}, value: ${value}")
      case None =>
        println(s"name: ${name} is not claimed before define")
    }
    this
  }

  def run(exp: Exp): Either[ErrorMsg, Exp] = {
    val env = ctx.toEnv
    val result = for {
      the <- exp.infer(ctx)
      typeValue <- the.t.eval(env)
      value <- exp.eval(env)
      norm <- value.readBack(ctx, typeValue)
    } yield The(the.t, norm)

    result match {
      case Right(exp) =>
        println(s"// ${exp}")
      case Left(ErrorMsg(msg)) =>
        println(s"error: ${msg}")
    }

    result
  }
}
