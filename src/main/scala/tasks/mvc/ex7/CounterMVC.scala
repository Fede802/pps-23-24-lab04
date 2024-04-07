package u04.monads

@main def runCounterMVC =
  import tasks.mvc.CounterStateImpl.*
  import Monads.*
  import Monad.*
  import States.*
  import State.*
  import tasks.mvc.WindowStateImpl.*
  import u03.extensionmethods.Streams.*

  def mv[SM, SV, AM, AV](m1: State[SM,AM], f: AM => State[SV,AV]): State[(SM,SV), AV] =
    State: (sm, sv) =>
      val (sm2, am) = m1.run(sm)
      val (sv2, av) = f(am).run(sv)
      ((sm2, sv2), av)

  def windowCreation(str: String): State[Window, Stream[String]] = for
    _ <- setSize(300, 300)
    _ <- addButton(text = "inc", name = "IncButton")
    _ <- addButton(text = "dec", name = "DecButton")
    _ <- addButton(text = "reset", name = "ResetButton")
    _ <- addTextField(text = "10", name = "CounterField")
    _ <- addButton(text = "set", name = "SetButton")
    _ <- addButton(text = "quit", name = "QuitButton")
    _ <- addLabel(text = str, name = "Label1")
    _ <- show()
    events <- eventStream()
  yield events

  val controller = for
    events <- mv(seq(reset(), get()), i => windowCreation(i.toString()))
    _ <- seqN(events.map(_ match
        case "IncButton" => mv(seq(inc(), get()), i => toLabel(i.toString, "Label1"))
        case "DecButton" => mv(seq(dec(), get()), i => toLabel(i.toString, "Label1"))
        case "ResetButton" => mv(seq(reset(), get()), i => toLabel(i.toString, "Label1"))
        case "SetButton" =>
          for
            text <- mv(nop(), i => textFromField("CounterField"))
            _ <- if text.toIntOption.isDefined
                    then mv(seq(set(text.toInt), get()), i => toLabel(i.toString, "Label1"))
                    else mv(get(), i => toLabel(i.toString, "Label1"))
          yield ()
        case "QuitButton" => mv(nop(), _ => exec(sys.exit()))))
  yield ()

  controller.run((initialCounter(), initialWindow))