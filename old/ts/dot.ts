import assert from "assert"
import * as ut from "../util"
import * as gs from "./game-semantics"
import { module_t } from "./core"
import { ref_t } from "./ref"
import { record_t } from "./record"

export
class dot_t extends gs.game_t {
  head: gs.game_t
  name: string

  constructor (
    head: gs.game_t,
    name: string,
  ) {
    super ()
    this.head = head
    this.name = name
  }

  copy (): dot_t {
    return new dot_t (
      this.head.copy (),
      this.name,
    )
  }

  deref (m: module_t): gs.game_t {
    throw new Error ("TODO")
    //     if (this.head instanceof record_t) {
    //       let record: record_t = this.head
    //       record.map.get (this.name)
    //       return this.module.game (this.name)
    //     } else if (this.head instanceof ref_t) {
    //     } else {
    //       // TODO
    //       // game_t should have a generic `.deref ()`
    //       throw new Error ("head of dot_t must be record_t of ref_t")
    //     }
  }

  choices (player: gs.player_t): Array <gs.choice_t> {
    throw new Error ("can not play dot_t")
  }

  choose (m: module_t, choice: gs.choice_t): gs.game_t {
    throw new Error ("can not play dot_t")
  }

  report (): object {
    // let game = this.deref ()
    // return game.report ()
    return {
      "kind": "dot_t",
      "head": this.head.report (),
      "name": this.name,
    }
  }
}
