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
import java.io.{ PrintStream, ByteArrayOutputStream }
import org.json4s.DefaultFormats
import java.util.Collections.EmptyList

class LoginSpec extends FlatSpec with Matchers with MockServer with BeforeAndAfter {
  implicit val formats = DefaultFormats

  var existSessionIdList = List.empty[String]
  var isNew: Option[Boolean] = None

  before {
    isNew = None
  }

  private def readAllFromReader(reader: java.io.Reader): String = {
    val sb = new StringBuffer()
    val buffer = Array[Char](1024)
    var nread = reader.read(buffer)
    while (nread >= 0) {
      sb.append(buffer, 0, nread)
      nread = reader.read(buffer)
    }
    sb.toString
  }

  protected val server: RunnableServer = {
    val random = new Random()

    unfiltered.netty.Server.http(9877).plan(
      // define the server behavior: return session_id on POST to /login
      unfiltered.netty.cycle.Planify {
        case req@POST(Path(Seg("login" :: Nil))) =>
          val body = readAllFromReader(req.reader)
          val reqSource = req.remoteAddr
          val maybeJson = org.json4s.native.JsonMethods.parseOpt(body)
          val maybeSession = maybeJson.flatMap(_.extractOpt[Session])

          maybeSession match {
            case None =>
              InternalServerError ~> ResponseString("Failed")
            case Some(session) if session.session_id.isDefined =>
              val sessionId = session.session_id.get
              if (session.`new`) {
                if (existSessionIdList.contains(sessionId)) {
                  Conflict ~> ResponseString("already exists session_id")
                } else {
                  val sessionIdJson = "{\"session_id\": \"" + sessionId + "\"}"
                  isNew = Option(session.`new`)
                  Created ~> ResponseString(sessionIdJson)
                }
              } else {
                if (sessionId.contains("FailedRequest")) {
                  InternalServerError ~> ResponseString("Failed")
                } else if (sessionId.contains("TimeoutTest")) {
                  Thread.sleep(1000)
                  val sessionIdJson = "{\"session_id\": \"" + sessionId + "\"}"
                  Created ~> ResponseString(sessionIdJson)
                } else if (existSessionIdList.contains(sessionId)) {
                  val sessionIdJson = "{\"session_id\": \"" + sessionId + "\"}"
                  isNew = Option(session.`new`)
                  Created ~> ResponseString(sessionIdJson)
                } else {
                  Unauthorized ~> ResponseString("Unknown session_id")
                }
              }

            case Some(session) =>
              isNew = Option(session.`new`)
              val sessionId = random.nextLong()
              val sessionIdJson = "{\"session_id\": \"" + sessionId + "\"}"

              Created ~> ResponseString(sessionIdJson)
          }
        case _ =>
          NotFound ~> ResponseString("404")
      })
  }

  "Sending an HTTP POST to the gateway server" should "return a session id" in {
    val sessionIdOption = JubaQLClient.getSessionId("localhost", 9877, -1, None, false)
    sessionIdOption should not be (None)
    isNew shouldBe Some(true)
  }

  it should "not return an empty session id" in {
    // create new session
    val sessionIdOption = JubaQLClient.getSessionId("localhost", 9877, -1, None, false)
    sessionIdOption should not be (None)
    sessionIdOption.get should not be ("")
    isNew shouldBe Some(true)
  }

  it should "return unknown session response when unknown session ID is specified" in {
    // connect session, not found sessionId
    val mockStderr = new ByteArrayOutputStream()
    val mockPrintStream = new PrintStream(mockStderr)
    val sessionIdOption = JubaQLClient.getSessionId("localhost", 9877, -1, Option("test"), true, mockPrintStream)

    sessionIdOption should be(None)
    mockStderr.toString should startWith("[ERROR/401] Unknown session_id")
    isNew shouldBe None
  }

  it should "return exists session response when existed session ID is specified" in {
    // connect session, existed sessionId
    existSessionIdList = existSessionIdList :+ "existsSession"
    val mockStderr = new ByteArrayOutputStream()
    val mockPrintStream = new PrintStream(mockStderr)
    val sessionIdOption = JubaQLClient.getSessionId("localhost", 9877, -1, Option("existsSession"), true, mockPrintStream)

    sessionIdOption should not be (None)
    sessionIdOption.get should be("existsSession")
    isNew shouldBe Some(false)
  }

  it should "[Create session] return exists session response when existed session ID is specified" in {
    // create session, specified sessionId
    val mockStderr = new ByteArrayOutputStream()
    val mockPrintStream = new PrintStream(mockStderr)
    val sessionIdOption = JubaQLClient.getSessionId("localhost", 9877, -1, Option("specifiedSession"), false, mockPrintStream)

    sessionIdOption should not be (None)
    sessionIdOption.get should be("specifiedSession")
    isNew shouldBe Some(true)
  }

  "Sending an HTTP POST to the gateway server" should "illegal url return error Response" in {
    val mockStderr = new ByteArrayOutputStream()
    val mockPrintStream = new PrintStream(mockStderr)
    JubaQLClient.getSessionId("localhost", 9877, -1, Option("FailedRequest"), true, mockPrintStream) should be(None)
    mockStderr.toString should startWith("[ERROR/500] Failed")
    isNew shouldBe None
  }

  it should "illegal combination of parameter" in {
    val mockStderr = new ByteArrayOutputStream()
    val mockPrintStream = new PrintStream(mockStderr)
    //illegal combination of parameter
    JubaQLClient.getSessionId("ww.localhost", 9877, -1, None, true, mockPrintStream) should be(None)
    mockStderr.toString should startWith("[ERROR] Internal Error. illegal argument: sessionId = None, reconnect flag = true")
    isNew shouldBe None
  }

  "Sending an HTTP POST to an unreachable server" should "return None" in {
    val mockStderr = new ByteArrayOutputStream()
    val mockPrintStream = new PrintStream(mockStderr)
    JubaQLClient.getSessionId("ww.localhost", 1234, -1, None, false, mockPrintStream) should be(None)
    mockStderr.toString should startWith("[ERROR] java.nio.channels.UnresolvedAddressException")
    isNew shouldBe None
  }

  it should "connect specific sessionId return None" in {
	    val mockStderr = new ByteArrayOutputStream()
	    val mockPrintStream = new PrintStream(mockStderr)
	    JubaQLClient.getSessionId("ww.localhost", 1234, -1, Option("test"), true, mockPrintStream) should be(None)
	    mockStderr.toString should startWith("[ERROR] java.nio.channels.UnresolvedAddressException")
	    isNew shouldBe None
	  }

	  it should "create specific sessionId return None" in {
	    val mockStderr = new ByteArrayOutputStream()
	    val mockPrintStream = new PrintStream(mockStderr)
	    JubaQLClient.getSessionId("ww.localhost", 1234, -1, Option("test"), false, mockPrintStream) should be(None)
	    mockStderr.toString should startWith("[ERROR] java.nio.channels.UnresolvedAddressException")
	    isNew shouldBe None
	  }

  "Sending an HTTP POST to the gateway server" should "success no timeout" in {
    val mockStderr = new ByteArrayOutputStream()
    val mockPrintStream = new PrintStream(mockStderr)
    JubaQLClient.getSessionId("localhost", 9877, 2000, Option("TimeoutTest"), false, mockPrintStream) should not be(None)
  }

  it should "error timeout" in {
    val mockStderr = new ByteArrayOutputStream()
    val mockPrintStream = new PrintStream(mockStderr)
    JubaQLClient.getSessionId("localhost", 9877, 100, Option("TimeoutTest"), true, mockPrintStream) should be(None)
    mockStderr.toString should startWith("[ERROR] java.util.concurrent.TimeoutException")
  }
}

case class Session(session_id: Option[String], `new`: Boolean)
