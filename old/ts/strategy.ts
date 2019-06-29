import assert from "assert"
import * as ut from "../util"
import * as gs from "./game-semantics"

/**
 * A strategy for a player,
 *   gives instruction for the player at each choice.
 * Imagining you are a game master in China,
 *   and you can not travel to Japan for a championship,
 *   so a friend have to play the championship on behalf of you,
 *   and you have to teach your strategy to your friend.
 * A strategy must thought of all the possible choices,
 *   specially those tough choices,
 *   induced by the opponent player's good moves.
 */
export
abstract class strategy_t {
  abstract player: gs.player_t
}

// TODO
// to win a game of game is to provide any game

/**
 * verifier's winning strategy is called verification (or proof).
 */
export
abstract class verification_t extends strategy_t {
  player: gs.player_t = "verifier"
}

/**
 * falsifier's winning strategy is called falsification.
 */
export
abstract class falsification_t extends strategy_t {
  player: gs.player_t = "falsifier"
}

/**
 * the verification of a union,
 *   is to provide a member of the union.
 */
export
class member_t extends verification_t {
  // TODO
}

/**
 * the verification of a record,
 *   is to fill in all the fields of the record.
 */
export
class fulfill_t extends verification_t {
  // TODO
}

/**
 * it is fun to verify an pi.
 * the verification of an pi,
 *   is a falsification of its ante,
 *   and a verification of its succ,
 *     with reference to the falsification of its ante.
 */
export
class fun_t extends verification_t {
  // TODO
}

// TODO
// strategy.wins (game: gs.game_t): true | Error
// game.use (strategy: strategy_t): test_t
