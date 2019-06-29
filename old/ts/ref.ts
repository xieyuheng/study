import assert from "assert"
import * as ut from "../util"
import * as gs from "./game-semantics"
import { module_t } from "./core"

export
class ref_t extends gs.game_t {
  name: string

  constructor (
    name: string,
  ) {
    super ()
    this.name = name
  }

  copy (): ref_t {
    return new ref_t (
      this.name,
    )
  }

  deref (m: module_t): gs.game_t {
    return m.game (this.name)
  }

  choices (player: gs.player_t): Array <gs.choice_t> {
    throw new Error ("can not play ref_t")
  }

  choose (m: module_t, choice: gs.choice_t): gs.game_t {
    throw new Error ("can not play ref_t")
  }

  report (): string {
    // let game = this.deref ()
    // return game.report ()
    return this.name
  }
}
