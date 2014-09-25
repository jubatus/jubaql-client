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

import scala.util.Random
import scala.sys.process._
import org.scalatest._
import unfiltered.response._
import unfiltered.util.RunnableServer
import unfiltered.request.{Body, POST, Seg, Path}
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._
import org.json4s.JsonAST.JString
import unfiltered.response.ResponseString
import java.nio.file.{Paths, Files}

/** Tests the correct behavior with respect to command lien parameters.
  *
  * We want to run the application with parameters in a separate process
  * and capture the output and return code. We use the sbt-start-script
  * plugin <https://github.com/sbt/sbt-start-script> to run the application,
  * since running with `sbt run` leads to significant overhead and makes
  * dealing with output complicated.
  */
class ParameterSpec extends FlatSpec with Matchers with MockServer {

  override protected def beforeAll(): Unit = {
    // if there is no script to start the application yet, generate it
    if (!Files.exists(Paths.get("target/start"))) {
      Seq("sbt", "start-script").!
    }
    super.beforeAll()
  }

  protected val server: RunnableServer = {
    // generate a session id for this communication
    val random = new Random()
    val sessionId = random.nextLong().toString
    val sessionIdJson = "{\"session_id\": \"" + sessionId + "\"}"

    unfiltered.netty.Server.http(9877).plan(
      // define the server behavior
      unfiltered.netty.cycle.Planify {
        // return session_id on POST to /login
        case req@POST(Path(Seg("login" :: Nil))) =>
          Created ~> ResponseString(sessionIdJson)
        // return some result if a query is given with valid session id
        case req@POST(Path(Seg("jubaql" :: Nil))) =>
          val body = new String(Body.bytes(req), "UTF-8")
          parseOpt(body) match {
            case Some(bodyData) =>
              val JString(reqSessionId) = bodyData \ "session_id"
              val JString(query) = bodyData \ "query"
              if (reqSessionId == sessionId && !query.isEmpty) {
                val result = ("result" -> "OK")
                Created ~> ResponseString(compact(render(result)))
              } else {
                val result = ("error" -> "invalid query")
                InternalServerError ~> ResponseString(compact(render(result)))
              }
            case None =>
              InternalServerError ~> ResponseString("error")
          }
        case _ =>
          NotFound ~> ResponseString("404")
      })
  }

  /**
   * Returns a ProcessLogger logging stdout/stderr to two StringBuffers.
   */
  protected def getProcessLogger(): (ProcessLogger, StringBuffer, StringBuffer) = {
    val stdoutBuffer = new StringBuffer()
    val stderrBuffer = new StringBuffer()
    val logger = ProcessLogger(line => {
      stdoutBuffer append line
      stdoutBuffer append "\n"
    },
      line => {
        stderrBuffer append line
        stderrBuffer append "\n"
      })
    (logger, stdoutBuffer, stderrBuffer)
  }

  "Passing a non-existing script file name" should "print an error and exit" in {
    // start the client and specify a file that does (probably) not exit
    val command = Seq("target/start", "-f", "xyz")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    // check exit code and console output
    exitCode shouldBe 1
    stdoutBuffer.toString shouldBe ""
    stderrBuffer.toString shouldBe "Given script file \"xyz\" does not exist.\n"
  }

  "Passing a script file name" should "login and execute the commands therein" in {
    // start the client and specify a file with JubaQL statements
    val command = Seq("target/start", "-f", "src/test/resources/test.jubaql")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    // check exit code and console output
    exitCode shouldBe 0
    stdoutBuffer.toString shouldBe "OK\nOK\n"
    stderrBuffer.toString should startWith("Using session id")
  }
}
