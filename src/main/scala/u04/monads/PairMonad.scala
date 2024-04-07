package u04.monads

import u04.monads.Monads.Monad
import u04.monads.PairMonad.Pair
import u04.monads.Sequences.Sequence

object PairMonad:

  case class Pair[A](a1: A, a2: A)

  given stateMonad: Monad[[A] =>> Pair[A]] with
    override def unit[A](a: A): Pair[A] = Pair(a,a)
    extension[A] (m: Pair[A])
      override def flatMap[B](f: A => Pair[B]): Pair[B] =
        Pair(f(m.a1).a2, f(m.a2).a2)


@main def runPair =
  val res =
    for
      a <- Pair(10, 20)
      _ <- Pair(a, a*2)
    yield ()

  println(Pair(10, 20).flatMap(a => Pair(a, a*2)))

  println(res) // Pair(20,40)