package xieyuheng.tartlet

object Arrow {
  def apply(
    arg_t: Exp,
    ret_t: Exp,
  ): Type = Pi("_", arg_t, ret_t)
}
