package tasks.mvc

import u04.monads.States.State

trait DrawMyNumberState:
  enum Status:
    case WIN, LOSE, RUNNING
  type GameState
  def newGame(): GameState
  def hint(): State[GameState, String]
  def status(): State[GameState, Status]
  def guess(g: Int): State[GameState, Unit]
  def reset(): State[GameState, Unit] = State(i => (newGame(), ()))
  def nop(): State[GameState, Unit] = State(i => (i, ()))

object DrawMyNumberStateImpl extends DrawMyNumberState:
  private val initialAttempt = 10;
  private case class DrawMyNumberGameState(
    maxAttempt: Int,
    numAttempt: Int,
    numToGuess: Int,
    hint: String,
    state: Status = Status.RUNNING
  )
  opaque type GameState = DrawMyNumberGameState
  extension (g: GameState) private def update(hint: String, status: Status): (GameState, Unit) =
    (DrawMyNumberGameState(g.maxAttempt, g.numAttempt + 1, g.numToGuess, hint, status), ())

  override def newGame(): GameState =
    DrawMyNumberGameState(initialAttempt, 0, 300, s"Insert a number, ${initialAttempt} attempts left")

  override def hint(): State[GameState, String] =
    State(i => (i, i.hint))

  override def status(): State[GameState, Status] =
    State(i => (i, i.state))

  override def guess(g: Int): State[GameState, Unit] = State(i => {
    if (i.numAttempt >= i.maxAttempt || i.state == Status.WIN) (i, ())
    else {
      val updatedAttempt = i.numAttempt + 1
      var newHint =
        if (g == i.numToGuess) "You won!"
        else if (g < i.numToGuess) s"Insert a higher number, ${initialAttempt-updatedAttempt} attempts left"
        else s"Insert a lower number, ${initialAttempt-updatedAttempt} attempts left"
      val newStatus =
        if (g == i.numToGuess) Status.WIN
        else if (updatedAttempt == i.maxAttempt) {newHint = "You Lose!";Status.LOSE}
        else Status.RUNNING
      i.update(newHint, newStatus)
    }
  })


