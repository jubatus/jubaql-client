// Jubatus: Online machine learning framework for distributed environment
// Copyright (C) 2014-2015 Preferred Networks and Nippon Telegraph and Telephone Corporation.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License version 2.1 as published by the Free Software Foundation.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
package us.jubat.jubaql_client

import org.scalatest._
import unfiltered.response._
import unfiltered.util.RunnableServer
import unfiltered.request.{POST, Seg, Path}
import scala.util.Random
import java.io.{PrintStream, ByteArrayOutputStream}

class LoginSpec extends FlatSpec with Matchers with MockServer {

  protected val server: RunnableServer = {
    val random = new Random()
    unfiltered.netty.Server.http(9877).plan(
      // define the server behavior: return session_id on POST to /login
      unfiltered.netty.cycle.Planify {
        case req@POST(Path(Seg("login" :: Nil))) =>
          val sessionId = random.nextLong()
          val sessionIdJson = "{\"session_id\": \"" + sessionId + "\"}"
          Created ~> ResponseString(sessionIdJson)
        case _ =>
          NotFound ~> ResponseString("404")
      })
  }

  "Sending an HTTP POST to the gateway server" should "return a session id" in {
    val sessionIdOption = JubaQLClient.getSessionId("localhost", 9877)
    sessionIdOption should not be (None)
  }

  it should "not return an empty session id" in {
    val sessionIdOption = JubaQLClient.getSessionId("localhost", 9877)
    sessionIdOption should not be (None)
    sessionIdOption.get should not be ("")
  }

  "Sending an HTTP POST to an unreachable server" should "return None" in {
    val mockStderr = new ByteArrayOutputStream()
    val mockPrintStream = new PrintStream(mockStderr)
    JubaQLClient.getSessionId("ww.localhost", 1234, mockPrintStream) should be(None)
    mockStderr.toString should startWith("[ERROR] java.nio.channels.UnresolvedAddressException")
  }
}

