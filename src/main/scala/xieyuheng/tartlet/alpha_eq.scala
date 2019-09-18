package xieyuheng.tartlet

import java.util.UUID

object alpha_eq {

  def apply(
    x: Exp,
    y: Exp,
    x_map: Map[String, String],
    y_map: Map[String, String],
  ): Boolean =
    alpha_eq(x, y, x_map, y_map)

  def alpha_eq(
    x: Exp,
    y: Exp,
    x_map: Map[String, String],
    y_map: Map[String, String],
  ): Boolean = {
    x match {
      case Var(name: String) =>
        y match {
          case Var(name2) =>
            (x_map.get(name), x_map.get(name2)) match {
              case (Some(sym), Some(sym2)) =>
                sym == sym2
              case (None, None) =>
                name == name2
              case _ =>
                false
            }
          case _ =>
            false
        }
      case Atom() =>
        y match {
          case Atom() => true
          case _ => false
        }
      case Quote(sym: String) =>
        y match {
          case Quote(sym2) => sym == sym2
          case _ => false
        }
      case Eqv(t: Exp, from: Exp, to: Exp) =>
        y match {
          case Eqv(t2, from2, to2) => {
            alpha_eq(t, t2, x_map, y_map) &&
            alpha_eq(from, from2, x_map, y_map) &&
            alpha_eq(to, to2, x_map, y_map)
          }
          case _ => false
        }
      case Replace(target: Exp, motive: Exp, base: Exp) =>
        y match {
          case Replace(target2, motive2, base2) =>
            alpha_eq(target, target2, x_map, y_map) &&
            alpha_eq(motive, motive2, x_map, y_map) &&
            alpha_eq(base, base2, x_map, y_map)
          case _ => false
        }
      case Same() =>
        y match {
          case Same() => true
          case _ => false
        }
      case Succ(prev: Exp) =>
        y match {
          case Succ(prev2) =>
            alpha_eq(prev, prev2, x_map, y_map)
          case _ => false
        }
      case NatInd(target: Exp, motive: Exp, base: Exp, step: Exp) =>
        y match {
          case NatInd(target2, motive2, base2, step2) =>
            alpha_eq(target, target2, x_map, y_map) &&
            alpha_eq(motive, motive2, x_map, y_map) &&
            alpha_eq(base, base2, x_map, y_map) &&
            alpha_eq(step, step2, x_map, y_map)
          case _ => false
        }
      case Nat() =>
        y match {
          case Nat() => true
          case _ => false
        }
      case Zero() =>
        y match {
          case Zero() => true
          case _ => false
        }
      case Ap(rator: Exp, rand: Exp) =>
        y match {
          case Ap(rator2, rand2) =>
            alpha_eq(rator, rator2, x_map, y_map) &&
            alpha_eq(rand, rand2, x_map, y_map)
          case _ => false
        }
      case Fn(name: String, body: Exp) =>
        y match {
          case Fn(name2, body2) => {
            val sym = UUID.randomUUID().toString
            alpha_eq(body, body2, x_map + (name -> sym), y_map + (name2 -> sym))
          }
          case _ => false
        }
      case Absurd() =>
        y match {
          case Absurd() => true
          case _ => false
        }
      case AbsurdInd(target: Exp, motive: Exp) =>
        y match {
          case AbsurdInd(target2, motive2) =>
            alpha_eq(target, target2, x_map, y_map) &&
            alpha_eq(motive, motive2, x_map, y_map)
          case _ => false
        }
      case Sigma(name: String, arg_t: Exp, dep_t: Exp) =>
        y match {
          case Sigma(name2, arg_t2, dep_t2) => {
            val sym = UUID.randomUUID().toString
            alpha_eq(arg_t, arg_t2, x_map, y_map) &&
            alpha_eq(dep_t, dep_t2, x_map + (name -> sym), y_map + (name -> sym))
          }
          case _ => false
        }
      case Sole() =>
        y match {
          case Sole() => true
          case _ => false
        }
      case Trivial() =>
        y match {
          case Trivial() => true
          case _ => false
        }
      case Universe() =>
        y match {
          case Universe() => true
          case _ => false
        }
      case Pi(name: String, arg_t: Exp, dep_t: Exp) =>
        y match {
          case Pi(name2, arg_t2, dep_t2) => {
            val sym = UUID.randomUUID().toString
            alpha_eq(arg_t, arg_t2, x_map, y_map) &&
            alpha_eq(dep_t, dep_t2, x_map + (name -> sym), y_map + (name -> sym))
          }
          case _ => false
        }
      case Car(pair: Exp) =>
        y match {
          case Car(pair2) =>
            alpha_eq(pair, pair2, x_map, y_map)
          case _ => false
        }
      case Cdr(pair: Exp) =>
        y match {
          case Cdr(pair2) =>
            alpha_eq(pair, pair2, x_map, y_map)
          case _ => false
        }
      case Cons(car: Exp, cdr: Exp) =>
        y match {
          case Cons(car2, cdr2) =>
            alpha_eq(car, car2, x_map, y_map) &&
            alpha_eq(cdr, cdr2, x_map, y_map)
          case _ => false
        }
      case The(t: Exp, value: Exp) =>
        y match {
          case The(t2, value2) =>
            (t, t2) match {
              case (Absurd(), Absurd()) =>
                true
              case _ =>
                if (alpha_eq(t, t2, x_map, y_map)
                  && alpha_eq(value, value2, x_map, y_map)) {
                  true
                } else {
                  false
                }
            }
          case _ =>
            false
        }

    }
  }
}
