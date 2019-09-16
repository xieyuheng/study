package xieyuheng.tartlet

import java.util.UUID

object alpha_eq {

  def apply(
    fn: Exp,
    that: Exp,
    this_map: Map[String, String],
    that_map: Map[String, String],
  ): Boolean =
    alpha_eq(fn, that, this_map, that_map)

  def alpha_eq(
    fn: Exp,
    that: Exp,
    this_map: Map[String, String],
    that_map: Map[String, String],
  ): Boolean = {
    fn match {
      case Var(name: String) =>
        that match {
          case Var(name2) =>
            (this_map.get(name), this_map.get(name2)) match {
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
      case Atom =>
        that match {
          case Atom => true
          case _ => false
        }
      case Quote(sym: String) =>
        that match {
          case Quote(sym2) => sym == sym2
          case _ => false
        }
      case Eqv(t: Exp, from: Exp, to: Exp) =>
        that match {
          case Eqv(t2, from2, to2) => {
            alpha_eq(t, t2, this_map, that_map) &&
            alpha_eq(from, from2, this_map, that_map) &&
            alpha_eq(to, to2, this_map, that_map)
          }
          case _ => false
        }
      case Replace(target: Exp, motive: Exp, base: Exp) =>
        that match {
          case Replace(target2, motive2, base2) =>
            alpha_eq(target, target2, this_map, that_map) &&
            alpha_eq(motive, motive2, this_map, that_map) &&
            alpha_eq(base, base2, this_map, that_map)
          case _ => false
        }
      case Same =>
        that match {
          case Same => true
          case _ => false
        }
      case Succ(prev: Exp) =>
        that match {
          case Succ(prev2) =>
            alpha_eq(prev, prev2, this_map, that_map)
          case _ => false
        }
      case NatInd(target: Exp, motive: Exp, base: Exp, step: Exp) =>
        that match {
          case NatInd(target2, motive2, base2, step2) =>
            alpha_eq(target, target2, this_map, that_map) &&
            alpha_eq(motive, motive2, this_map, that_map) &&
            alpha_eq(base, base2, this_map, that_map) &&
            alpha_eq(step, step2, this_map, that_map)
          case _ => false
        }
      case Nat =>
        that match {
          case Nat => true
          case _ => false
        }
      case Zero =>
        that match {
          case Zero => true
          case _ => false
        }
      case Ap(rator: Exp, rand: Exp) =>
        that match {
          case Ap(rator2, rand2) =>
            alpha_eq(rator, rator2, this_map, that_map) &&
            alpha_eq(rand, rand2, this_map, that_map)
          case _ => false
        }
      case Fn(name: String, body: Exp) =>
        that match {
          case Fn(name2, body2) => {
            val sym = UUID.randomUUID().toString
            alpha_eq(body, body2, this_map + (name -> sym), that_map + (name2 -> sym))
          }
          case _ => false
        }
      case Absurd =>
        that match {
          case Absurd => true
          case _ => false
        }
      case AbsurdInd(target: Exp, motive: Exp) =>
        that match {
          case AbsurdInd(target2, motive2) =>
            alpha_eq(target, target2, this_map, that_map) &&
            alpha_eq(motive, motive2, this_map, that_map)
          case _ => false
        }
      case Sigma(name: String, arg_t: Exp, cdr_t: Exp) =>
        that match {
          case Sigma(name2, arg_t2, cdr_t2) => {
            val sym = UUID.randomUUID().toString
            alpha_eq(arg_t, arg_t2, this_map, that_map) &&
            alpha_eq(cdr_t, cdr_t2, this_map + (name -> sym), that_map + (name -> sym))
          }
          case _ => false
        }
      case Sole =>
        that match {
          case Sole => true
          case _ => false
        }
      case Trivial =>
        that match {
          case Trivial => true
          case _ => false
        }
      case Universe =>
        that match {
          case Universe => true
          case _ => false
        }
      case Pi(name: String, arg_t: Exp, ret_t: Exp) =>
        that match {
          case Pi(name2, arg_t2, ret_t2) => {
            val sym = UUID.randomUUID().toString
            alpha_eq(arg_t, arg_t2, this_map, that_map) &&
            alpha_eq(ret_t, ret_t2, this_map + (name -> sym), that_map + (name -> sym))
          }
          case _ => false
        }
      case Car(pair: Exp) =>
        that match {
          case Car(pair2) =>
            alpha_eq(pair, pair2, this_map, that_map)
          case _ => false
        }
      case Cdr(pair: Exp) =>
        that match {
          case Cdr(pair2) =>
            alpha_eq(pair, pair2, this_map, that_map)
          case _ => false
        }
      case Cons(car: Exp, cdr: Exp) =>
        that match {
          case Cons(car2, cdr2) =>
            alpha_eq(car, car2, this_map, that_map) &&
            alpha_eq(cdr, cdr2, this_map, that_map)
          case _ => false
        }
      case The(t: Exp, value: Exp) =>
        that match {
          case The(t2, value2) =>
            (t, t2) match {
              case (Absurd, Absurd) =>
                true
              case _ =>
                if (alpha_eq(t, t2, this_map, that_map)
                  && alpha_eq(value, value2, this_map, that_map)) {
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
