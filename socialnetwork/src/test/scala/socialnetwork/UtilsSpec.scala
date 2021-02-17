package socialnetwork

import socialnetwork.utils.Type._
import socialnetwork.commands.Commands._
import socialnetwork.utils.Util._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UtilsSpec extends AnyFlatSpec with Matchers {

  val testMesage= ("testMessage", System.currentTimeMillis())
  val testUser = "user1"

  "The Util object" should "not parse an erroneous command" in {
    parseCommand("failed command") equals None
  }
  it should "parse a correct command" in {
    parseCommand(testUser) equals Some(Read(testUser))
  }

  it should "pretty print a message" in {
    prettyPrint(testMesage) should include (s"> ${testMesage._1}")
  }

  it should "pretty print a message with usename" in {
    prettyPrintWithUser(testUser, testMesage) should include (s"> ${testUser} - ${testMesage._1}")
  }
}
