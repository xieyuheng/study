package xieyuheng.cicada

import eval._
import pretty._

object api {

  def top_list_eval(top_list: List[Top]): Either[Err, Unit] = {
    var local_env = Env()
    for {
      _ <- util.list_foreach_maybe_err(top_list) {
        case TopLet(name, exp) =>
          eval(local_env, exp).map {
            case value =>
              local_env = local_env.ext(name, value)
              println(s"let ${name} = ${pretty_exp(exp)}")
              println(s">>> ${name} = ${pretty_value(value)}")
          }
        case TopDefine(name, t, exp) =>
          eval(local_env, t).map {
            case t_value =>
              eval(local_env, exp).map {
                case value =>
                  local_env = local_env.ext(name, value)
                  println(s"define ${name} : ${pretty_exp(t)} = ${pretty_exp(exp)}")
                  println(s">>>>>> ${name} : ${pretty_value(t_value)} = ${pretty_value(value)}")
              }
          }
      }
    } yield ()
  }

  // def top_list_check
}
