package xieyuheng.simple

object simple extends App {
  import eval._

  // TODO
  // with global mutable name_exp_map before @show a exp, expend global name first

  val f = Fn("x", None, Var("x", None))
  val a = Var("a", None)

  println(beta_step(Ap(f, Ap(f, Ap(f, a)))))
  println(beta_step(beta_step(Ap(f, Ap(f, Ap(f, a))))))
}
