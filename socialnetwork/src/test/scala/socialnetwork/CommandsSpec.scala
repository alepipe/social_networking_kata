package socialnetwork

import socialnetwork.utils.Type._
import socialnetwork.utils.Util._
import socialnetwork.commands.Commands._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandsSpec extends AnyFlatSpec with Matchers {
  // exec wall

  val mockedState: State = Set(
    ("user1", Set(), List(("message1", System.currentTimeMillis))),
    ("user2", Set("user1"), List(("message2", System.currentTimeMillis)))
  )

  "The Commands object" should "exec Post command" in {
    val testUser = "user1"
    val testMessage = "testMessage"
    val result = execCommand(mockedState, Post(testUser, testMessage))._1
      .filter(user => user._1 == testUser)
      .flatMap(user => user._3)
      .map(message => message._1)

    result should contain (testMessage)
  }

  it should "exec Read command" in {
    val testUser = "user1"
    val result = execCommand(mockedState, Read(testUser))._2
    val expectedOutput = mockedState
      .filter(user => user._1 == testUser)
      .flatMap(user => user._3)
      .map(prettyPrint)
      .mkString("\n")

    result shouldBe expectedOutput
  }

  it should "exec Follow command" in {
    val testUser1 = "user1"
    val testUser2 = "user2"
    val result = execCommand(mockedState, Follow(testUser1, testUser2))._1
      .filter(user => user._1 == testUser1)
      .flatMap(user => user._2)

    result should contain (testUser2)
  }

  it should "exec Wall command" in {
    val testUser = "user2"
    val result = execCommand(mockedState, Wall(testUser))._2

    val expectedOutput = mockedState
      .map({ case (user, _, messages) => (user, messages) })
      .flatMap({ case (user, messages) =>
        messages.map(message => (user, message))
      })
      .toList
      .sortBy(_._2._2)
      .map(elem => prettyPrintWithUser(elem._1, elem._2))
      .mkString("\n")

    result shouldBe expectedOutput
  }
}
