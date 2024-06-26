package tasks.mvc

import u03.extensionmethods.Streams.*
import u04.monads.Monads.Monad.seqN
import u04.monads.States.State

trait WindowState:
  type Window
  def initialWindow: Window
  def setSize(width: Int, height: Int): State[Window, Unit]
  def addButton(text: String, name: String): State[Window, Unit]
  def addLabel(text: String, name: String): State[Window, Unit]
  def addTextField(text: String, name: String): State[Window, Unit]
  def toLabel(text: String, name: String): State[Window, Unit]
  def show(): State[Window, Unit]
  def exec(cmd: =>Unit): State[Window, Unit]
  def eventStream(): State[Window, Stream[String]]

object WindowStateImpl extends WindowState:
  import tasks.mvc.SwingFunctionalFacade.*
  
  type Window = Frame
  
  
  def initialWindow: Window = createFrame

  def setSize(width: Int, height: Int): State[Window, Unit] = 
    State(w => ((w.setSize(width, height)), {}))
  def addButton(text: String, name: String): State[Window, Unit] =
    State(w => ((w.addButton(text, name)), {}))
  def addLabel(text: String, name: String): State[Window, Unit] =
    State(w => ((w.addLabel(text, name)), {}))
  def addTextField(text: String, name: String): State[Window, Unit] =
    State(w => ((w.addTextField(text, name)), {}))
  def toLabel(text: String, name: String): State[Window, Unit] =
    State(w => ((w.showToLabel(text, name)), {}))
  def textFromField(name: String): State[Window, String] =
    State(w => (w, w.textFromField(name)))
  def show(): State[Window, Unit] =
    State(w => (w.show, {}))
  def resetWindow(): State[Window, Unit] =
    State(w => (w.resetWindow(),{}))
  def disable(name: String): State[Window, Unit] =
    State(w => (w.disable(name),{}))
  def exec(cmd: =>Unit): State[Window, Unit] =
    State(w => (w, cmd))  
  def eventStream(): State[Window, Stream[String]] =
    State(w => (w, Stream.generate(() => w.events().get)))
  
@main def windowStateExample =
  import WindowStateImpl.*
  import u03.extensionmethods.Streams.*
  import u04.*

  val windowCreation = for 
    _ <- setSize(300, 300)
    _ <- addButton(text = "inc", name = "IncButton")
    _ <- addButton(text = "dec", name = "DecButton")
    _ <- addButton(text = "quit", name = "QuitButton")
    _ <- addLabel(text = "-", name = "Label1")
    _ <- show()
    e <- eventStream()
  yield e

  val windowEventsHandling = for
    _ <- windowCreation
    e <- eventStream()
    _ <- seqN(e.map(_ match
        case "IncButton" => toLabel("i", "Label1")
        case "DecButton" => toLabel("d", "Label1")
        case "QuitButton" => exec(sys.exit())))
  yield ()

  windowEventsHandling.run(initialWindow)