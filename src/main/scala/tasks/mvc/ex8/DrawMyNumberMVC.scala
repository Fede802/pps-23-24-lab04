package tasks.mvc

import tasks.mvc.DrawMyNumberStateImpl.*
import u03.extensionmethods.Streams.Stream
import u04.monads.States.State
import tasks.mvc.WindowStateImpl.*
import u04.monads.Monads.Monad.{seq, seqN}

@main def runDrawMyNumberMVC =

  def mv[SM, SV, AM, AV](m1: State[SM, AM], f: AM => State[SV, AV]): State[(SM, SV), AV] =
    State: (sm, sv) =>
      val (sm2, am) = m1.run(sm)
      val (sv2, av) = f(am).run(sv)
      ((sm2, sv2), av)

  def windowCreation(str: String): State[Window, Stream[String]] = for
    _ <- setSize(250, 250)
    _ <- addLabel(text = str, name = "Label1")
    //number to guess is fixed and hardcoded as hint in text field for debug purpose
    _ <- addTextField(text = "300", name = "GuessField")
    _ <- addButton(text = "guess", name = "GuessButton")
    _ <- addButton(text = "reset", name = "ResetButton")
    _ <- addButton(text = "quit", name = "QuitButton")
    _ <- show()
    events <- eventStream()
  yield events

  val controller = for
    events <- mv(seq(reset(), hint()), h => windowCreation(h))
    _ <- seqN(events.map(_ match
      case "GuessButton" =>
        for
          text <- mv(nop(), _ => textFromField("GuessField"))
          _ <- if text.toIntOption.isDefined
            then mv(seq(guess(text.toInt), hint()), h => toLabel(h, "Label1"))
            else mv(hint(), i => toLabel(i.toString, "Label1"))
          _ <- mv(status(), s =>
            if (s != Status.RUNNING)
            then seq(disable("GuessButton"), disable("GuessField"))
            else exec(())
          )
        yield ()
      case "ResetButton" => mv(seq(reset(), hint()), h => seq(resetWindow(),toLabel(h, "Label1")))
      case "QuitButton" => mv(nop(), _ => exec(sys.exit()))))
  yield ()

  controller.run((newGame(), initialWindow))