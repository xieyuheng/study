package xieyuheng.eopl.lang_proc

case class Err(msg: String) {
  def append_cause(cause: Err): Err = {
    Err(
      msg ++
        "because:\n" ++
        cause.msg)
  }
}
