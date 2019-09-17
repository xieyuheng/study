// package xieyuheng.tartlet

// case class Module(
//   var ctx: Ctx = Ctx(),
// ) {
//   def claim(name: String, t: Exp): Module = {
//     if (ctx.lookup_type(name).isDefined) {
//       println(s"name: ${name} is alreay claimed")
//     } else {
//       t.check(ctx, ValUniverse()) match {
//         case Right(checkedTypeExp) =>
//           for {
//             checkedType <- checkedTypeExp.eval(ctx.to_env)
//           } ctx = ctx.ext(name, Bind(checkedType))
//         case Left(errorMsg) =>
//           println(s"type check fail, name: ${name}, errorMsg: ${errorMsg}")
//       }
//     }
//     this
//   }

//   def define(name: String, exp: Exp): Module = {
//     ctx.lookup_den(name) match {
//       case Some(Bind(t_val)) =>
//         exp.check(ctx, t_val) match {
//           case Right(exp) =>
//             for {
//               value <- exp.eval(ctx.to_env)
//             } ctx = ctx.ext(name, Def(t_val, value))
//           case Left(errorMsg) =>
//             println(s"type check fail for name: ${name}, errorMsg: ${errorMsg}")
//         }
//       case Some(Def(t_val, value)) =>
//         println(s"name: ${name} is already defined, type: ${t_val}, value: ${value}")
//       case None =>
//         println(s"name: ${name} is not claimed before define")
//     }
//     this
//   }

//   def run(exp: Exp): Either[Err, Exp] = {
//     val env = ctx.to_env
//     val result = for {
//       the <- exp.infer(ctx)
//       t_val <- the.t.eval(env)
//       value <- exp.eval(env)
//       norm <- value.readback_val(ctx, t_val)
//     } yield The(the.t, norm)

//     result match {
//       case Right(exp) =>
//         println(s"==> ${exp}")
//       case Left(Err(msg)) =>
//         println(s"error: ${msg}")
//     }

//     result
//   }
// }
