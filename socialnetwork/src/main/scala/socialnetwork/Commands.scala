package socialnetwork.commands

import socialnetwork.utils.Type.{User, State}
import socialnetwork.utils.Util._

object Commands {

  sealed trait Command
  case class Post(user: String, message: String) extends Command
  case class Read(user: String) extends Command
  case class Follow(follower: String, followed: String) extends Command
  case class Wall(user: String) extends Command

  def execCommand(state: State, command: Command): (State, String) =
    command match {
      case command: Post   => execPost(state, command)
      case command: Read   => execRead(state, command)
      case command: Follow => execFollow(state, command)
      case command: Wall   => execWall(state, command)
    }

  private def execPost(state: State, post: Post): (State, String) = {
    val newstate: State =
      if (state.exists((user => user._1 == post.user))) {
        for (user <- state) yield {
          user match {
            case (username, follower, messages) if (username == post.user) => {
              (
                username,
                follower,
                messages.appended(post.message, System.currentTimeMillis)
              )
            }

            case (username, follower, messages) => {
              (username, follower, messages)
            }
          }
        }
      } else {
        state + (
          (
            post.user,
            Set(),
            List((post.message, System.currentTimeMillis()))
          )
        )
      }
    return (newstate, "")
  }

  private def execRead(state: State, read: Read): (State, String) = {
    val string: String =
      state.find(user => user._1 == read.user) match {
        case Some((user, follower, messages)) =>
          messages.map(prettyPrint).mkString("\n")
        case None => s"> User ${read.user} not found!"
      }
    return (state, string)
  }

  private def execWall(state: State, wall: Wall): (State, String) = {
    val currentTime: Long = System.currentTimeMillis()
    val string: String =
      state.find(user => user._1 == wall.user) match {
        case Some((user, following, messages)) =>
          (List((user, messages)) ++
            state
              .filter({ case (user, _, _) => following.contains(user) })
              .map({ case (user, _, messages) => (user, messages) }))
            .flatMap({ case (user, messages) =>
              messages.map(message => (user, message))
            })
            .sortBy(_._2._2)
            .map(elem => prettyPrintWithUser(elem._1, elem._2))
            .mkString("\n")
        case None => s"> User ${wall.user} not found!"
      }
    return (state, string)
  }

  private def execFollow(
      state: State,
      follow: Follow
  ): (State, String) = {
    val newstate: (State, String) =
      state.find(user => user._1 == follow.follower) match {
        case Some((user, _, _)) =>
          state.find(user => user._1 == follow.followed) match {
            case Some(_) =>
              (
                for (user <- state) yield {
                  user match {
                    case (user, following, messages) =>
                      if (user == follow.follower) {
                        (user, following + follow.followed, messages)
                      } else { (user, following, messages) }
                  }
                },
                ""
              )

                    case None => (state, s"> User ${follow.followed} not found!")
          }
            case None => (state, s"> User ${follow.follower} not found!")
      }
    return (newstate)
  }
}
