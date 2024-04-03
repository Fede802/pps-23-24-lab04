package u04.monads
import u04.monads.Monads.*

object States:

  // data structure for state (evolution)
  // a state is/has a function evolving S and producing a result A
  case class State[S, A](run: S => (S, A))

  // minimal set of algorithms
  object State:
    // a facility to run the state on an initial `s`
    extension [S, A](m: State[S, A])
      def apply(s: S): (S, A) = m match
        case State(run) => run(s)

  // define a given the works on all S, shall use "type lambdas"
  given stateMonad[S]: Monad[[A] =>> State[S, A]] with
    // unit: a state with no evolution, just the result
    def unit[A](a: A): State[S, A] = State(s => (s, a))

    // flatMap: runs the state, use result to create a new state
    extension [A](m: State[S, A])
      override def flatMap[B](f: A => State[S, B]): State[S, B] =
        State(s =>
          println(s"s: $s")
          m.apply(s) match
            case (s2, a) =>
              println(s"s2: $s2 a: $a")
              val r = f(a).apply(s2)
              println("r "+r)
              r
        )

@main def runMVC2 =
  import Monads.*
  import States.*
  import State.*

  def modelview[STATOMODEL, STATOVIEW, AZIONEMODEL, AZIONEVIEW](
      m1: State[STATOMODEL, AZIONEMODEL],
      f: AZIONEMODEL => State[STATOVIEW, AZIONEVIEW]
  ): State[(STATOMODEL, STATOVIEW), AZIONEVIEW] =
    State: (statomodel, statoview) =>
      val (statomodel2, azionemodel) = m1.run(statomodel)
      val (statoview2, azioneview) = f(azionemodel).run(statoview)
      ((statomodel2, statoview2), azioneview)

  def incAndGet(): State[Int, Int] = State(i => (i + 1, i))
  def getTextFieldText(): State[Unit, String] = State(i => ({}, "ciao"))
  def print(s: String): State[Unit, String] = State(i => ({}, s"[$s]"))
//  def mv1 = modelview(incAndGet(), i => State(i => (i, s"[$i]")))
//  println(mv1)
//  println(mv1.run(10, 20)) // ((11,20),[20])
  val t = for
    text <- modelview(incAndGet(), _ => getTextFieldText())
    res <-  if text.toIntOption.isDefined
    then modelview(incAndGet(), text => print(text.toString))
    else modelview(incAndGet(), text => print("null"))
  yield (res)
  println(t.run(10, 1))
  println("before computation")
//  println(modelview(incAndGet(), _ => getTextFieldText()).flatMap(tt => modelview(incAndGet(), tt => print)));
//
//  val v1 = modelview(incAndGet(), _ => getTextFieldText())
//  val v2 = modelview(incAndGet(), tt2 => print)
//  val v3 = v2.map( _ => v1)







//  val initialState: State[Int, Int] = State(i => (i, i * 2))
//  val initialState2: State[Int, Int] = State(i => (i, i * 2))
//  val updateView: Int => State[Int, String] = i => State(i => (i, s"[$i]"))
//  println("aa")
//  val testforyy: State[(Any, Any), Any]  =
//    for
//      a <- modelview(initialState, updateView)
//      b <- modelview(initialState2, i => State(i => (i, s"[$i][$a]")))
//    yield b // ((10,20),20)
//  println("aa")
//  println(testforyy.run(10, 20)) // ((10,20),20)
//  println("aa")
////  val initialState: State[Int, Int] = State(i => (i, i * 2))
////  println(initialState.run(10)) // ((2,2),[2])
////  val updateView: Int => State[Int, String] = i => State(i => (i, s"[$i]"))
//////  println(result.run(1,10)) // ((2,2),[2])
////  val result = modelview(initialState, updateView)
//////  val result2 = modelview(result, updateView)
////  println(result.run(2, 10)) // ((2,2),[2])
