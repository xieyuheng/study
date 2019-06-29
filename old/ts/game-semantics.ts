import assert from "assert"
import * as ut from "../util"
import { module_t } from "./core"

/**
 * Game semantics framework for logic and type system.

 * constrains:
 * - two players -- verifier and falsifier
 * - normal play
 * - no draw
 * - the order of play is not strict
 *   - sometimes we can swap the order of local plays
 * - different choices have different effect -- monomorphism
 * - different players have different choices
 *   - choice belong to player,
 *     given a choice, we know which player is playing the choice.
 */

export
abstract class game_builder_t {
  abstract build (): game_t
}

export
abstract class game_t {
  abstract choices (player: player_t): Array <choice_t>
  abstract choose (m: module_t, choice: choice_t): game_t

  abstract copy <G extends game_t> (): game_t
  abstract report (): any

  info (label: any): this {
    console.group (label)
    ut.log (this.report ())
    console.groupEnd ()
    return this
  }

  loss_p (player: player_t): boolean {
    /**
     * normal play -- the last player who has no choice loss the game.
     */
    let choices = this.choices (player)
    return choices.length === 0
  }

  end_p (): boolean {
    return this.loss_p ("verifier") || this.loss_p ("falsifier")
  }
}

export
type player_t = "verifier" | "falsifier"

export
function opponent_player (player: player_t) {
  return player === "verifier"
    ? "falsifier"
    : "verifier"
}

export
abstract class choice_t {
  abstract report (): any
}
