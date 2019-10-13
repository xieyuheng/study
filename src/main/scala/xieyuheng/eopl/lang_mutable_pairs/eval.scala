package xieyuheng.eopl.lang_mutable_pairs

import pretty._

import xieyuheng.util.err._

object eval {

  def eval(exp: Exp, env: Env, store: Store): Either[Err, (Store, Val)] = {
    exp match {

      case Var(name: String) =>
        env.lookup_val(name) match {
          case Some(value) =>
            value match {
              case ValRef(address) =>
                store.ref_get(address) match {
                  case Some(value) =>
                    Right(store, value)
                  case None =>
                    Left(Err(
                      s"[eval fail]\n" ++
                        s"undefined address: ${address}\n"
                    ))
                }
              case _ =>
                Right(store, value)
            }
          case None =>
            Left(Err(
              s"[eval fail]\n" ++
                s"undefined name: ${name}\n"
            ))
        }

      case Num(num: Int) =>
        Right(store, ValNum(num))

      case Diff(exp1: Exp, exp2: Exp) =>
        val result = eval(exp1, env, store).flatMap {
          case (store1, val1) =>
            eval(exp2, env, store1).flatMap {
              case (store2, val2) =>
                (val1, val2) match {
                  case (ValNum(x), ValNum(y)) =>
                    Right(store2, ValNum(x - y))
                  case (x, y) =>
                    Left(Err(
                      s"[eval fail]\n" ++
                        s"diff(x, y) type mismatch\n" ++
                        s"expecting number\n" ++
                        s"x: ${pretty_val(x)}\n" ++
                        s"y: ${pretty_val(y)}\n"
                    ))
                }
            }
        }
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case ZeroP(exp1: Exp) =>
        val result = eval(exp1, env, store).flatMap {
          case (store1, val1) =>
            val1 match {
              case ValNum(x) =>
                if (x == 0) {
                  Right(store1, ValBool(true))
                } else {
                  Right(store1, ValBool(false))
                }
              case x =>
                Left(Err(
                  s"[eval fail]\n" ++
                    s"zero_p(x, y) type mismatch\n" ++
                    s"expecting number\n" ++
                    s"x: ${pretty_val(x)}\n"
                ))
            }
        }
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case If(exp1: Exp, exp2: Exp, exp3: Exp) =>
        val result = eval(exp1, env, store).flatMap {
          case (store1, val1) =>
            val1 match {
              case ValBool(true) =>
                eval(exp2, env, store1)
              case ValBool(false) =>
                eval(exp3, env, store1)
              case x =>
                Left(Err(
                  s"[eval fail]\n" ++
                    s"if x then _ else _ type mismatch\n" ++
                    s"expecting bool\n" ++
                    s"x: ${pretty_val(x)}\n"
                ))
            }
        }
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case Let(name: String, exp1: Exp, body: Exp) =>
        val result = eval(exp1, env, store).flatMap {
          case (store1, val1) =>
            val (store2, address) = store1.ref_new(val1)
            eval(body, env.ext_let(name, ValRef(address)), store2)
        }
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case Fn(name, body) =>
        Right(store, ValFn(name, body, env))

      case Ap(target, arg) =>
        val result = eval(target, env, store).flatMap {
          case (store1, f) =>
            eval(arg, env, store1).flatMap {
              case (store2, v) =>
                f match {
                  case f: ValFn =>
                    eval(f.body, f.env.ext_let(f.name, v), store2)
                  case _ =>
                    Left(Err(
                      s"[eval fail]\n" ++
                        s"f(x) type mismatch\n" ++
                        s"expecting function\n" ++
                        s"f: ${pretty_val(f)}\n"
                    ))
                }
            }
        }
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      // NOTE this can also be implemented by assignment
      case LetRec(fn_name, arg_name, fn_body, body) =>
        val result = eval(body, env.ext_let_rec(fn_name, arg_name, fn_body), store)
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      // NOTE this can also be implemented by assignment
      case LetRecMutual(map, body) =>
        val result = eval(body, env.ext_let_rec_mutual(map), store)
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case Sole() =>
        Right(store, ValSole())

      case Do(exp1: Exp, body: Exp) =>
        val result = eval(exp1, env, store).flatMap {
          case (store1, _) =>
            eval(body, env, store1)
        }
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case Assign(name: String, exp1: Exp) =>
        env.lookup_val(name) match {
          case Some(value) =>
            value match {
              case ValRef(address) =>
                eval(exp1, env, store).flatMap {
                  case (store1, val1) =>
                    val store2 = store1.ref_set(address, val1)
                    Right(store2, ValSole())
                }
              case x =>
                Left(Err(
                  s"[eval fail]\n" ++
                    s"set x = y type mismatch\n" ++
                    s"expecting x to be ref\n" ++
                    s"x: ${pretty_val(x)}\n"
                ))
            }
          case None =>
            Left(Err(
              s"[eval fail]\n" ++
                s"undefined name: ${name}\n"
            ))
        }

      case PairNew(exp1: Exp, exp2: Exp) =>
        val result = eval(exp1, env, store).flatMap {
          case (store1, val1) =>
            eval(exp2, env, store1).flatMap {
              case (store2, val2) =>
                val (store3, address_fst) = store2.ref_new(val1)
                val (store4, address_snd) = store3.ref_new(val2)
                Right(store4, ValPair(
                  ValRef(address_fst),
                  ValRef(address_snd)))
            }
        }
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case PairFst(exp1: Exp) =>
        val result = eval(exp1, env, store).flatMap {
          case (store1, val1) =>
            val1 match {
              case ValPair(ValRef(address), snd) =>
                store1.ref_get(address) match {
                  case Some(value) =>
                    Right(store1, value)
                  case None =>
                    Left(Err(
                      s"[eval fail]\n" ++
                        s"undefined address: ${address}\n"
                    ))
                }
              case x =>
                Left(Err(
                  s"[eval fail]\n" ++
                    s"pair_fst(x) type mismatch\n" ++
                    s"expecting pair\n" ++
                    s"x: ${pretty_val(x)}\n"
                ))
            }
        }
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case PairSnd(exp1: Exp) =>
        val result = eval(exp1, env, store).flatMap {
          case (store1, val1) =>
            val1 match {
              case ValPair(fst, ValRef(address)) =>
                store1.ref_get(address) match {
                  case Some(value) =>
                    Right(store1, value)
                  case None =>
                    Left(Err(
                      s"[eval fail]\n" ++
                        s"undefined address: ${address}\n"
                    ))
                }
              case x =>
                Left(Err(
                  s"[eval fail]\n" ++
                    s"pair_snd(x) type mismatch\n" ++
                    s"expecting pair\n" ++
                    s"x: ${pretty_val(x)}\n"
                ))
            }
        }
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case PairSetFst(exp1: Exp, exp2: Exp) =>
        val result = eval(exp1, env, store).flatMap {
          case (store1, val1) =>
            val1 match {
              case ValPair(ValRef(address), snd) =>
                eval(exp2, env, store1).flatMap {
                  case (store2, val2) =>
                    val store3 = store2.ref_set(address, val2)
                    Right(store3, ValSole())
                }
              case x =>
                Left(Err(
                  s"[eval fail]\n" ++
                    s"pair_set_fst(x, y) type mismatch\n" ++
                    s"expecting x to be pair\n" ++
                    s"x: ${pretty_val(x)}\n"
                ))
            }
        }
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case PairSetSnd(exp1: Exp, exp2: Exp) =>
        val result = eval(exp1, env, store).flatMap {
          case (store1, val1) =>
            val1 match {
              case ValPair(fst, ValRef(address)) =>
                eval(exp2, env, store1).flatMap {
                  case (store2, val2) =>
                    val store3 = store2.ref_set(address, val2)
                    Right(store3, ValSole())
                }
              case x =>
                Left(Err(
                  s"[eval fail]\n" ++
                    s"pair_set_snd(x, y) type mismatch\n" ++
                    s"expecting x to be pair\n" ++
                    s"x: ${pretty_val(x)}\n"
                ))
            }
        }
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case AssertEq(exp1, exp2) =>
        val result = eval(exp1, env, store).flatMap {
          case (store1, val1) =>
            eval(exp2, env, store1).flatMap {
              case (store2, val2) =>
                if (val1 == val2) {
                  Right(store2, ValSole())
                } else {
                  Left(Err(
                    s"[assert_eq fail]\n" ++
                      s">>> ${pretty_exp(exp1)}\n" ++
                      s"=== ${pretty_val(val1)}\n" ++
                      s">>> ${pretty_exp(exp2)}\n" ++
                      s"=== ${pretty_val(val2)}\n"
                  ))
                }
            }
        }
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

      case Show(exp1) =>
        val result = eval(exp1, env, store).flatMap {
          case (store1, val1) =>
            println(s">>> ${pretty_exp(exp1)}")
            println(s"=== ${pretty_val(val1)}")
            Right(store1, ValSole())
        }
        result_maybe_err(result, Err(
          s"[eval fail]\n" ++
            s"exp: ${pretty_exp(exp)}\n"
        ))

    }
  }

}
