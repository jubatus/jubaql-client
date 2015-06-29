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
import unfiltered.request.{Body, POST, Seg, Path}
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._
import org.json4s.JsonAST.JString
import java.io.{PrintStream, ByteArrayOutputStream}

class SendCommandSpec extends FlatSpec with Matchers with MockServer {

  protected val server: RunnableServer = {
    unfiltered.netty.Server.http(9877).plan(
      // define the server behavior: return session_id on POST to /login
      unfiltered.netty.cycle.Planify {
        case req@POST(Path(Seg("jubaql" :: Nil))) =>
          val body = new String(Body.bytes(req), "UTF-8")
          parseOpt(body) match {
            case Some(bodyData) =>
              val JString(sessionId) = bodyData \ "session_id"
              val JString(query) = bodyData \ "query"
              if (sessionId == "1234" && !query.isEmpty) {
                // simulate good session and good query
                val result = ("result" -> "OK")
                Created ~> ResponseString(compact(render(result)))
              } else if (sessionId == "1234" && query.isEmpty) {
                // simulate good session and bad query
                val result = ("error" -> "empty query")
                BadRequest ~> ResponseString(compact(render(result)))
              } else if (sessionId == "-1") {
                // simulate non-JSON answer
                Created ~> ResponseString("error")
              } else {
                // simulate bad session
                val result = ("error" -> "bad session id")
                Unauthorized ~> ResponseString(compact(render(result)))
              }
            case None =>
              InternalServerError ~> ResponseString("xyz")
          }
        case _ =>
          NotFound ~> ResponseString("404")
      })
  }

  "Sending a command with a valid session" should "print the result" in {
    val mockStdout = new ByteArrayOutputStream()
    val mockPrintStream = new PrintStream(mockStdout)
    JubaQLClient.handleCommand("localhost", 9877, "1234", "TRAIN jubatus",
      mockPrintStream)
    val expected = "OK\n"
    mockStdout.toString should be(expected)
  }

  "Sending a bad command with a valid session" should "print an error" in {
    val mockStdout = new ByteArrayOutputStream()
    val mockPrintStream = new PrintStream(mockStdout)
    JubaQLClient.handleCommand("localhost", 9877, "1234", "",
      mockPrintStream)
    mockStdout.toString should startWith("[ERROR/400]")
  }

  "Sending a command with a valid session to a non-existing server" should
    "print an error" in {
    val mockStdout = new ByteArrayOutputStream()
    val mockPrintStream = new PrintStream(mockStdout)
    JubaQLClient.handleCommand("ww.localhost", 9877, "1234", "TRAIN jubatus",
      mockPrintStream)
    mockStdout.toString should startWith(
      "[ERROR] java.nio.channels.UnresolvedAddressException")
  }

  "Sending a command with a valid session to a closed port" should
    "print an error" in {
    val mockStdout = new ByteArrayOutputStream()
    val mockPrintStream = new PrintStream(mockStdout)
    JubaQLClient.handleCommand("localhost", 1, "1234", "TRAIN jubatus",
      mockPrintStream)
    mockStdout.toString should startWith(
      "[ERROR] java.net.ConnectException: Connection refused")
  }

  "Sending a command with an invalid session" should "print an error" in {
    val mockStdout = new ByteArrayOutputStream()
    val mockPrintStream = new PrintStream(mockStdout)
    JubaQLClient.handleCommand("localhost", 9877, "123", "TRAIN jubatus",
      mockPrintStream)
    mockStdout.toString should startWith("[ERROR/401]")
  }

  "Receiving invalid JSON" should "print an error" in {
    val mockStdout = new ByteArrayOutputStream()
    val mockPrintStream = new PrintStream(mockStdout)
    // hack to get the mock server to send non-JSON
    JubaQLClient.handleCommand("localhost", 9877, "-1", "TRAIN jubatus",
      mockPrintStream)
    mockStdout.toString should startWith(
      "[ERROR] failed to parse JSON")
  }
}

