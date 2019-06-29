import assert from "assert"
import * as ut from "../util"
import * as gs from "./game-semantics"
import { module_t } from "./core"

/**
 * type of types, game of games.
 */
export
class type_t extends gs.game_t {
  /**
   * `type_t` is like a dynamic union,
   *   thus its player is verifier.
   */
  choices (player: gs.player_t): Array <gs.choice_t> {
    throw new Error ("TODO")
  }

  choose (m: module_t, choice: gs.choice_t): gs.game_t {
    throw new Error ("TODO")
  }

  copy (): type_t {
    return new type_t ()
  }

  report (): object {
    return {
      "kind": "type_t",
      // TODO
    }
  }
}
