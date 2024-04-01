package tasks.typeclasses

import u03.Optionals.Optional
import u03.Sequences.*
import u03.Sequences.Sequence.*

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:


//
//  def logAll[A](seq: Sequence[A]): Unit = seq match
//    case Cons(h, t) => log(h); logAll(t)
//    case _ => ()

//  trait Traversable[T[_]]:
//    def logAll[A](t: T[A], logger: A => Unit): Unit
//
//
//  given Traversable[Optional] with {
//    def logAll[A](opt: Optional[A], logger: A => Unit): Unit = opt match
//      case Optional.Just(a) => logger(a)
//      case _ => ()
//  }
//
//  given Traversable[Sequence] with {
//    def logAll[A](seq: Sequence[A], logger: A => Unit): Unit = seq match
//      case Cons(h, t) => logger(h); logAll(t, logger)
//      case _ => ()
//  }

  def log[A](a: A): Unit = println("The next element is: "+a)

  trait Traversable2[T[_]]:
    def logAll[A](t: T[A]): Unit


  given Traversable2[Optional] with {
    def logAll[A](opt: Optional[A]): Unit = opt match
      case Optional.Just(a) => log(a)
      case _ => ()
  }

  given Traversable2[Sequence] with {
    def logAll[A](seq: Sequence[A]): Unit = seq match
      case Cons(h, t) => log(h); logAll(t)
      case _ => ()
  }

  def logAll[B, A[_]: Traversable2](t: A[B]): Unit = summon[Traversable2[A]].logAll(t)

@main def main(): Unit = {
  import Ex5Traversable.{*, given}
  val seq = Cons(1, Cons(2, Cons(3, Nil())))
  val opt = Optional.Just(42)

//  logAll(seq)
  logAll(seq)
  logAll(opt)
}
