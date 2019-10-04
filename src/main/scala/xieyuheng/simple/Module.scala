package xieyuheng.simple

case class Module() {

  var top_list: List[Top] = List()

  def eval_with_global(exp: Exp, global: Map[String, Exp]): Exp = {
    val exp2 = eval.expend_global_variables(exp, global, Set())
    eval.beta_eta_reduction(exp2)
  }

  def run(): Unit = {
    var global: Map[String, Exp] = Map()

    top_list.foreach {
      case TopDecl(DeclLet(name, exp)) =>
        val exp2 = eval_with_global(exp, global)
        global = global + (name -> exp2)
      case TopShow(exp) =>
        show(exp, global)
      // case TopEq(e1, e2) =>
      //   assert_eq(e1, e2)
      // case TopNotEq(e1, e2) =>
      //   assert_not_eq(e1, e2)
      case _ => {}
    }
  }

//   def assert_not_eq(e1: Exp, e2: Exp): Unit = {
//     val v1 = eval(e1, env)
//     val v2 = eval(e2, env)
//     val n1 = readback_val(v1, Set())
//     val n2 = readback_val(v2, Set())
//     if (v1 == v2) {
//       println(s"[assertion fail]")
//       println(s"the following two expressions are asserted to be not equal")
//       println(s">>> ${pretty_exp(e1)}")
//       println(s"=== ${pretty_val(v1)}")
//       println(s"=== ${pretty_exp(n1)}")
//       println(s">>> ${pretty_exp(e2)}")
//       println(s"=== ${pretty_val(v2)}")
//       println(s"=== ${pretty_exp(n2)}")
//       throw new Exception()
//     }
//   }

//   def assert_eq(e1: Exp, e2: Exp): Unit = {
//     val v1 = eval(e1, env)
//     val v2 = eval(e2, env)
//     val n1 = readback_val(v1, Set())
//     val n2 = readback_val(v2, Set())
//     if (v1 != v2) {
//       println(s"[assertion fail]")
//       println(s"the following two expressions are asserted to be equal")
//       println(s">>> ${pretty_exp(e1)}")
//       println(s"=== ${pretty_val(v1)}")
//       println(s"=== ${pretty_exp(n1)}")
//       println(s">>> ${pretty_exp(e2)}")
//       println(s"=== ${pretty_val(v2)}")
//       println(s"=== ${pretty_exp(n2)}")
//       throw new Exception()
//     }
//   }

  def show(exp: Exp, global: Map[String, Exp]): Unit = {
    val value = eval_with_global(exp, global)
    println(s">>> ${exp}")
    println(s"=== ${value}")
    // val norm = readback_val(value, Set())
    // println(s">>> ${pretty_exp(exp)}")
    // println(s"=== ${pretty_val(value)}")
    // println(s"=== ${pretty_exp(norm)}")
    println()
  }

}
