import assert from "assert"
import * as ut from "../util"
import * as gs from "./game-semantics"
import { union_t } from "./union"
import { record_t } from "./record"
import { pi_t, arg_t, ret_t } from "./pi"
import { path_t, step_t } from "./path"
import { member_t } from "./member"
import { field_t } from "./field"

// Top level API of game semantics of cicada language.

export
function path (steps: Array <step_t>): path_t {
  return new path_t (steps)
}

export
let step = {
  member: (name: string) => new member_t (name),
  field: (name: string) => new field_t (name),
  arg: (name: string) => new arg_t (name),
  ret: () => new ret_t (),
}

export
type den_t = gs.game_t | gs.game_builder_t

export
class module_t {
  name: string
  den_map: Map <string, den_t>

  constructor (
    name: string,
    den_map: Map <string, den_t> = new Map (),
  ) {
    this.name = name
    this.den_map = den_map
  }

  /**
   * due to the design of `ref_t`,
   *   we can not imp deep copy here.
   */
  shallow_copy (): module_t {
    return new module_t (
      this.name,
      new Map (this.den_map),
    )
  }

  define (name: string, den: den_t): this {
    this.den_map.set (name, den)
    return this
  }

  game (name: string): gs.game_t {
    let den = this.den_map.get (name)
    if (den === undefined) {
      throw new Error (`unknown type of den: ${typeof den}, name: ${name}`)
    }

    if (den instanceof gs.game_t) {
      let game = den
      return game.copy ()
    } else if (den instanceof gs.game_builder_t) {
      let game_builder = den
      return game_builder.build ()
    } else {
      throw new Error (`unknown type of den: ${typeof den}, name: ${name}`)
    }
  }
}
