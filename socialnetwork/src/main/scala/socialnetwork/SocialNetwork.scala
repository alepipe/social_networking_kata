package socialnetwork

import socialnetwork.commands.Commands._
import socialnetwork.utils.Type._
import socialnetwork.utils.Util._
import socialnetwork.utils.Extractor

object Main {

  def mainLoop(state: State): Unit = {

    import scala.io.StdIn.readLine

    print("> ")
    readLine() match {
      case Extractor.exit()  => println("> Goodbye!")
      case Extractor.empty() => mainLoop(state)
      case input =>
        parseCommand(input) match {
          case None =>
            println("> Command not recognised!")
            mainLoop(state)
          case Some(command) =>
            val (newstate, output): (State, String) =
              execCommand(state, command)
            if (!output.isEmpty)
              println(output)
            mainLoop(newstate)
        }
    }
  }
}

object SocialNetwork extends App {
  Main.mainLoop(Set[User]())
}
