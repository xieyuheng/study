package xieyuheng.tartlet

case class Module(
  var ctx: Ctx = Ctx(),
) {
  def claim(name: String, t: Exp): Module = {
    if (ctx.lookup_type(name).isDefined) {
      println(s"name: ${name} is alreay claimed")
    } else {
      t.check(ctx, ValUniverse) match {
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
      case Some(Bind(typeVal)) =>
        exp.check(ctx, typeVal) match {
          case Right(exp) =>
            for {
              value <- exp.eval(ctx.toEnv)
            } ctx = ctx.ext(name, Def(typeVal, value))
          case Left(errorMsg) =>
            println(s"type check fail for name: ${name}, errorMsg: ${errorMsg}")
        }
      case Some(Def(typeVal, value)) =>
        println(s"name: ${name} is already defined, type: ${typeVal}, value: ${value}")
      case None =>
        println(s"name: ${name} is not claimed before define")
    }
    this
  }

  def run(exp: Exp): Either[Err, Exp] = {
    val env = ctx.toEnv
    val result = for {
      the <- exp.infer(ctx)
      typeVal <- the.t.eval(env)
      value <- exp.eval(env)
      norm <- value.readback_val(ctx, typeVal)
    } yield The(the.t, norm)

    result match {
      case Right(exp) =>
        println(s"==> ${exp}")
      case Left(Err(msg)) =>
        println(s"error: ${msg}")
    }

    result
  }
}
