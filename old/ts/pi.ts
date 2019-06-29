import assert from "assert"
import * as ut from "../util"
import * as gs from "./game-semantics"
import { path_t, step_t } from "./path"
import { ref_t } from "./ref"
import { module_t } from "./core"

export
class arg_t extends step_t {
  name: string

  constructor (name: string) {
    super ()
    this.name = name
  }

  forward (game: gs.game_t): gs.game_t {
    if (game instanceof pi_t) {
      let pi = game
      let next_game = pi.args.get (this.name)
      if (next_game === undefined) {
        throw new Error (`unknown arg name: ${this.name}`)
      } else {
        return next_game
      }
    } else {
      throw new Error ("arg_t step only forward an pi_t")
    }
  }

  deref (m: module_t, game: gs.game_t) {
    throw new Error ("can not deref arg_t")
  }

  repr (): any {
    return "@" + this.name
  }
}

export
class ret_t extends step_t {
  constructor () {
    super ()
  }

  forward (game: gs.game_t): gs.game_t {
    if (game instanceof pi_t) {
      let pi = game
      return pi.ret
    } else {
      throw new Error ("ret_t step only forward an pi_t")
    }
  }

  deref (m: module_t, game: gs.game_t) {
    throw new Error ("can not deref ret_t")
  }

  repr (): any {
    return "$ret"
  }
}

/**
 * `pi_t` has two stages: `args` and `ret`,
 *   during `args` player's roles are reversed.
 */
export
class pi_t extends gs.game_t {
  args: Map <string, gs.game_t>
  ret: gs.game_t

  constructor (
    args: Map <string, gs.game_t> | { [key: string]: gs.game_t },
    ret: gs.game_t,
  ) {
    super ()
    if (args instanceof Map) {
      this.args = args
    } else {
      this.args = ut.obj2map (args)
    }
    this.ret = ret
  }

  copy (): pi_t {
    return new pi_t (
      ut.mapmap (this.args, game => game.copy ()),
      this.ret.copy (),
    )
  }

  choices (player: gs.player_t): Array <gs.choice_t> {
    // TODO
    return []
  }

  choose (m: module_t, path: path_t): this {
    let next: gs.game_t = this
    for (let step of path.prefix ()) {
      next = step.forward (next)
    }
    path.target () .deref (m, next)
    return this
  }

  report (): object {
    let args: { [key: string]: any } = {}
    for (let [name, game] of this.args.entries ()) {
      args [name] = game.report ()
    }
    return {
      "kind": "pi_t",
      "args": args,
      "ret": this.ret.report (),
      "end": this.end_p (),
    }
  }
}
