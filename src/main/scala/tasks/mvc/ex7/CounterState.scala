package tasks.mvc


import u04.monads.Monads.Monad.seq
import u04.monads.States.State

trait CounterState:
  type Counter
  def initialCounter(): Counter
  def inc(): State[Counter, Unit]
  def dec(): State[Counter, Unit]
  def reset(): State[Counter, Unit]
  def get(): State[Counter, Int]
  def nop(): State[Counter, Unit]

object CounterStateImpl extends CounterState:
  opaque type Counter = Int
  def initialCounter(): Counter = 0
  def inc(): State[Counter, Unit] = State(i => (i + 1, ()));
  def dec(): State[Counter, Unit] = State(i => (i - 1, ()));
  def reset(): State[Counter, Unit] = State(i => (0, ()));
  def get(): State[Counter, Int] = State(i => (i, i));
  def set(n: Int): State[Counter, Unit] = State(i => (n, ()));
  def nop(): State[Counter, Unit] = State(i => (i, ()));
