package xieyuheng.eopl.lang_letrec

case class Err(msg: String) {
  def append_cause(cause: Err): Err = {
    Err(
      msg ++
        "because:\n" ++
        cause.msg)
  }
}
