//package u04.monads

//import u04.monads.States.State
//
////import u04.monads.IOs.IO
////import u04.monads.IOs.IO.{compute, read, write}
//trait DrawMyNumberState:
//  type GameIteration
//  def startGame(a:Int, g: Int): GameIteration
//  def nextGuess(g: Int): GameIteration
////  def inc(): State[Counter, Unit]
////  def dec(): State[Counter, Unit]
////  def reset(): State[Counter, Unit]
////  def get(): State[Counter, Int]
////  def nop(): State[Counter, Unit]
//
//object DrawMyNumberStateImpl extends DrawMyNumberState:
//
//
//  opaque type GameIteration = State[Int, Int]
//
//  override def startGame(a:Int, g: Int): GameIteration = State[]
//
//  override def nextGuess(g: Int): GameIteration = ???
//
////  def initialCounter(): Counter = 0
//
//  // giving (new_counter, result)
////  def inc(): State[Counter, Unit] = State(i => (i + 1, ()));
////  def dec(): State[Counter, Unit] = State(i => (i - 1, ()));
////  def reset(): State[Counter, Unit] = State(i => (0, ()));
////  def get(): State[Counter, Int] = State(i => (i, i));
////  def set(n: Int): State[Counter, Unit] = State(i => (n, ()));
////  def nop(): State[Counter, Unit] = State(i => (i, ()));
//
//@main def tryDrawMyNumber =
//  import Monads.*, Monad.*, States.*, State.*, CounterStateImpl.*, WindowStateImpl.*
//  import u03.extensionmethods.Streams.*
//
//  def mv[SM, SV, AM, AV](m1: State[SM, AM], f: AM => State[SV, AV]): State[(SM, SV), AV] =
//    State: (sm, sv) =>
//      val (sm2, am) = m1.run(sm)
//      val (sv2, av) = f(am).run(sv)
//      ((sm2, sv2), av)
//
//  enum Result:
//    case Won, Lost
//
//  def windowCreation(): State[Window, Stream[String]] = for
//    _ <- setSize(300, 300)
//    _ <- addLabel(text = "make first guess", name = "OutputLabel")
//    _ <- addButton(text = "submit", name = "SubmitButton")
//    _ <- addTextField(text = "", name = "GuessField")
//    _ <- show()
//    events <- eventStream()
//  yield events
//
//  val controller = for
//    events <- mv(seq(reset(), get()), i => windowCreation())
//    _ <- seqN(events.map(_ match
//      case "SubmitButton" => mv(seq(inc(), get()), i => toLabel(i.toString, "Label1"))
//      case "QuitButton" => mv(nop(), _ => exec(sys.exit()))))
//  yield ()
//
//  controller.run((initialCounter(), initialWindow))
//
////  def drawNumberGame(attempts: Int, draw: Int): IO[Result] =
////    if attempts == 0
////    then compute(Result.Lost)
////    else
////      for
////        _ <- write("give your number: ")
////        d <- read()
////        i <- compute(d.toInt)
////        res <-
////          if (i == draw)
////          then
////            for
////              _ <- write("won")
////              r <- compute(Result.Won)
////            yield r
////          else
////            for
////              _ <- write(if i > draw then "too high!" else "too low!")
////              r <- drawNumberGame(attempts - 1, draw)
////            yield r
////      yield res
////
////  // starts one run of the game
////  drawNumberGame(10, scala.util.Random.nextInt(100))
//
