package socialnetwork.utils

import socialnetwork.commands.Commands._

object Type {

  type Timestamp = Long
  type Message = (String, Timestamp)
  type User = (String, Set[String], List[Message])
  type State = Set[User]

}

object Extractor {

  val post = "(\\w+)\\s->\\s(.+)".r
  val follow = "(\\w+)\\sfollows\\s(\\w+)".r
  val wall = "(\\w+)\\swall".r
  val read = "(\\w+)".r
  val exit = "quit|exit".r
  val empty = "".r

}

object Util {

  def parseCommand(input: String): Option[Command] = {
    input.trim() match {
      case Extractor.wall(user) => Some(Wall(user))
      case Extractor.follow(follower, followed) =>
        Some(Follow(follower, followed))
      case Extractor.post(user, message) => Some(Post(user, message))
      case Extractor.read(user)          => Some(Read(user))
      case _                             => None
    }
  }

  def prettyPrint(message: Type.Message): String =
    s"> ${message._1} ${formatTime(message._2)}"

  def prettyPrintWithUser(user: String, message: Type.Message): String =
    prettyPrint(message).replace(">", s"> ${user} -")

  private def formatTime(timestamp: Type.Timestamp): String = {
    ((System.currentTimeMillis - timestamp) / 1000) match {
      case time if time == 1  => s"(1 second ago)"
      case time if time < 60  => s"(${time} seconds ago)"
      case time if time == 60 => s"(1 minute ago)"
      case time               => s"(${time / 60} minutes ago)"
    }
  }

}
