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
import org.json4s.JsonAST.{JString, JNothing, JBool}
import unfiltered.response.ResponseString
import java.nio.file.{Paths, Files}
import java.io.{PrintStream, BufferedReader, InputStreamReader, BufferedWriter, OutputStreamWriter, PrintWriter}

/** Tests the correct behavior with respect to command lien parameters.
  *
  * We want to run the application with parameters in a separate process
  * and capture the output and return code. We use the sbt-start-script
  * plugin <https://github.com/sbt/sbt-start-script> to run the application,
  * since running with `sbt run` leads to significant overhead and makes
  * dealing with output complicated.
  */
class ParameterSpec extends FlatSpec with Matchers with MockServer with BeforeAndAfter {

  val startScriptPath = "target/start"
  var isNew: Option[Boolean] = None

  before {
    isNew = None
  }

  override protected def beforeAll(): Unit = {
    // if there is no script to start the application yet, generate it
    if (!Files.exists(Paths.get(startScriptPath))) {
      Seq("sbt", "start-script").!
    }
    super.beforeAll()
  }

  protected val server: RunnableServer = {
    // generate a session id for this communication
    val random = new Random()
    var sessionId = random.nextLong().toString


    unfiltered.netty.Server.http(9877).plan(
      // define the server behavior
      unfiltered.netty.cycle.Planify {
        // return session_id on POST to /login
        case req@POST(Path(Seg("login" :: Nil))) =>
          val body = new String(Body.bytes(req), "UTF-8")
          parseOpt(body) match {
            case Some(bodyData) =>
               bodyData \ "session_id" match {
                 case JString(reqSessionId) =>
                   sessionId = reqSessionId
                 case JNothing =>
                   //nothing
                 case _ =>
                   InternalServerError ~> ResponseString("error")
               }
               bodyData \ "new" match {
                 case JBool(newFlag) => isNew = Option(newFlag)
                 case _ =>
                   InternalServerError ~> ResponseString("error")
               }
              val sessionIdJson = "{\"session_id\": \"" + sessionId + "\"}"
              Created ~> ResponseString(sessionIdJson)
            case None =>
              InternalServerError ~> ResponseString("error")
          }
        // return some result if a query is given with valid session id
        case req@POST(Path(Seg("jubaql" :: Nil))) =>
          val body = new String(Body.bytes(req), "UTF-8")
          parseOpt(body) match {
            case Some(bodyData) =>
              val JString(reqSessionId) = bodyData \ "session_id"
              val JString(query) = bodyData \ "query"
              if (query.startsWith("ERROR")) {
                InternalServerError ~> ResponseString("error")
              }
              else if (reqSessionId == sessionId && !query.isEmpty) {
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

  protected def getProcessIO(): (ProcessIO, StringBuffer, StringBuffer) = {
    val stdinBuffer = new StringBuffer()
    val stdoutBuffer = new StringBuffer()
    val pio = new ProcessIO(
      in => {
        val writer = new PrintWriter(new BufferedWriter(new OutputStreamWriter(in)))
        writer.println(stdinBuffer.toString())
        writer.flush()
      },
      out => {
        val reader = new BufferedReader(new InputStreamReader(out))
        def readLine(): Unit = {
          val line = reader.readLine()
          if (line != null) {
            stdoutBuffer append line
            stdoutBuffer append "\n"
            readLine()
          }
        }
        readLine()
      },
      err => {})
    (pio, stdinBuffer, stdoutBuffer)
  }

  "Passing a non-existing script file name" should "print an error and exit" in {
    // start the client and specify a file that does (probably) not exit
    val command = Seq(startScriptPath, "-f", "xyz")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    // check exit code and console output
    exitCode shouldBe 1
    stdoutBuffer.toString shouldBe "requestTimeout value is 300000 ms\n"
    stderrBuffer.toString shouldBe "Given script file \"xyz\" does not exist.\n"
  }

  "Passing a script file name" should "login and execute the commands therein" in {
    // start the client and specify a file with JubaQL statements
    val command = Seq(startScriptPath, "-f", "src/test/resources/test.jubaql")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    // check exit code and console output
    exitCode shouldBe 0
    stdoutBuffer.toString shouldBe "requestTimeout value is 300000 ms\nOK\nOK\n"
    stderrBuffer.toString should startWith("Using session id")
  }

  "Passing a timeout" should "value is default" in {
    val command = Seq("target/start", "-f", "xyz")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    stdoutBuffer.toString shouldBe "requestTimeout value is 300000 ms\n"
  }

  it should "value is 0" in {
    val command = Seq("target/start", "-f", "xyz", "-t", "0")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    stdoutBuffer.toString shouldBe "requestTimeout value is -1 ms\n"
  }

  it should "value is 10" in {
    val command = Seq("target/start", "-f", "xyz", "-t", "10")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    stdoutBuffer.toString shouldBe "requestTimeout value is 10000 ms\n"
  }

  "parseCommandlineOption" should "success timeout option" in {
    // 省略
    var result = JubaQLClient.parseCommandlineOption(Array())
    result.isDefined shouldBe true
    result.get.timeout.isEmpty shouldBe true

    // 最小値
    result = JubaQLClient.parseCommandlineOption(Array("-t", "0"))
    result.isDefined shouldBe true
    result.get.timeout shouldBe Some(0)

    result = JubaQLClient.parseCommandlineOption(Array("--timeout", "0"))
    result.isDefined shouldBe true
    result.get.timeout shouldBe Some(0)

    // 最大値
    result = JubaQLClient.parseCommandlineOption(Array("-t", "2145600"))
    result.isDefined shouldBe true
    result.get.timeout shouldBe Some(2145600)

    result = JubaQLClient.parseCommandlineOption(Array("--timeout", "2145600"))
    result.isDefined shouldBe true
    result.get.timeout shouldBe Some(2145600)
  }

  it should "error timeout option" in {
    var out = new java.io.ByteArrayOutputStream()
    Console.setErr(new PrintStream(out))
    // 値なし
    var result = JubaQLClient.parseCommandlineOption(Array("-t"))
    result.isEmpty shouldBe true
    out.toString() should include("-t <timeout> | --timeout <timeout>")
    out.toString() should include("request timeout value (default: 300sec)")
    out.toString() should include("Missing value after -t")

    out = new java.io.ByteArrayOutputStream()
    Console.setErr(new PrintStream(out))
    result = JubaQLClient.parseCommandlineOption(Array("--timeout"))
    result.isEmpty shouldBe true
    out.toString() should include("Missing value after --timeout")

    // 範囲外
    out = new java.io.ByteArrayOutputStream()
    Console.setErr(new PrintStream(out))
    result = JubaQLClient.parseCommandlineOption(Array("-t", "-1"))
    result.isEmpty shouldBe true
    out.toString() should include("""bad timeout value; timeout value n must be "0 <= n <= 2145600"""")

    out = new java.io.ByteArrayOutputStream()
    Console.setErr(new PrintStream(out))
    result = JubaQLClient.parseCommandlineOption(Array("--timeout", "-1"))
    result.isEmpty shouldBe true
    out.toString() should include("""bad timeout value; timeout value n must be "0 <= n <= 2145600"""")

    out = new java.io.ByteArrayOutputStream()
    Console.setErr(new PrintStream(out))
    result = JubaQLClient.parseCommandlineOption(Array("-t", "2145601"))
    result.isEmpty shouldBe true
    out.toString() should include("""bad timeout value; timeout value n must be "0 <= n <= 2145600"""")

    out = new java.io.ByteArrayOutputStream()
    Console.setErr(new PrintStream(out))
    result = JubaQLClient.parseCommandlineOption(Array("--timeout", "2145601"))
    result.isEmpty shouldBe true
    out.toString() should include("""bad timeout value; timeout value n must be "0 <= n <= 2145600"""")
  }

  "Passing a session_id parameter" should "print an error and exit" in {
    // start the client
    val command = Seq(startScriptPath, "-f", "src/test/resources/test.jubaql", "sessionId")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    // check exit code and console output
    exitCode shouldBe 1
    stderrBuffer.toString should startWith("Error: Unknown argument 'sessionId'")
  }

  "Passing a -s option" should "succeed login" in {
    // start the client
    val command = Seq(startScriptPath, "-f", "src/test/resources/test.jubaql", "-s" , "s")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    // check exit code and console output
    exitCode shouldBe 0
    stdoutBuffer.toString shouldBe "requestTimeout value is 300000 ms\nOK\nOK\n"
    stderrBuffer.toString should startWith("Using session id \"s\"")
    isNew shouldBe Some(true)
  }

  "Passing a --session option" should "succeed login" in {
    // start the client
    val command = Seq(startScriptPath, "-f", "src/test/resources/test.jubaql", "--session" , "session")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    // check exit code and console output
    exitCode shouldBe 0
    stdoutBuffer.toString shouldBe "requestTimeout value is 300000 ms\nOK\nOK\n"
    stderrBuffer.toString should startWith("Using session id \"session\"")
    isNew shouldBe Some(true)
  }

  "Passing a -s option non value" should "print an error and exit" in {
    // start the client
    val command = Seq(startScriptPath, "-f", "src/test/resources/test.jubaql", "-s")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    // check exit code and console output
    exitCode shouldBe 1
    stderrBuffer.toString should startWith("Error: Missing value after -s")
    isNew shouldBe None
  }

  "Passing a -s option empty value" should "print an error and exit" in {
    // start the client
    val command = Seq(startScriptPath, "-f", "src/test/resources/test.jubaql", "-s" , "")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    // check exit code and console output
    exitCode shouldBe 1
    stderrBuffer.toString should startWith("Error: bad session_id")
    isNew shouldBe None
  }

  "Passing a -r option" should "succeed login" in {
    // start the client
    val command = Seq(startScriptPath, "-f", "src/test/resources/test.jubaql", "-s" , "s", "-r")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    // check exit code and console output
    exitCode shouldBe 0
    stdoutBuffer.toString shouldBe "requestTimeout value is 300000 ms\nOK\nOK\n"
    stderrBuffer.toString should startWith("Using session id \"s\"")
    isNew shouldBe Some(false)
  }

  "Passing a --reconnect option" should "succeed login" in {
    // start the client
    val command = Seq(startScriptPath, "-f", "src/test/resources/test.jubaql", "-s" , "s", "--reconnect")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    // check exit code and console output
    exitCode shouldBe 0
    stdoutBuffer.toString shouldBe "requestTimeout value is 300000 ms\nOK\nOK\n"
    stderrBuffer.toString should startWith("Using session id \"s\"")
    isNew shouldBe Some(false)
  }

  "Passing a -r option with value" should "print an error and exit" in {
    // start the client
    val command = Seq(startScriptPath, "-f", "src/test/resources/test.jubaql", "-s" , "session", "-r", "value")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    // check exit code and console output
    exitCode shouldBe 1
    stderrBuffer.toString should startWith("Error: Unknown argument 'value'")
    isNew shouldBe None
  }

  //illegal combination of options(s,r)
  "Passing a -r option without -s" should "print an error and exit" in {
    // start the client
    val command = Seq(startScriptPath, "-f", "src/test/resources/test.jubaql", "-r")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    // check exit code and console output
    exitCode shouldBe 1
    stderrBuffer.toString should startWith("Error: -r option requires the -s option")
    isNew shouldBe None
  }

  "Passing a -e option" should "succeed exit on failure" in {
    // start the client
    val command = Seq(startScriptPath, "-f", "src/test/resources/test_err.jubaql", "-e")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    // check exit code and console output
    exitCode shouldBe 2
    stdoutBuffer.toString should startWith("requestTimeout value is 300000 ms\n")
    stdoutBuffer.toString should endWith("\"error\"\n")
  }

  it should "continue on failure" in {
    // start the client
    val command = Seq(startScriptPath, "-f", "src/test/resources/test_err.jubaql")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    // check exit code and console output
    exitCode shouldBe 0
    stdoutBuffer.toString should startWith("requestTimeout value is 300000 ms\n")
    stdoutBuffer.toString should include("\"error\"\n")
    stdoutBuffer.toString should endWith("OK\n")
  }

  "Passing a --exit-on-failure option" should "succeed exit on failure" in {
    // start the client
    val command = Seq(startScriptPath, "-f", "src/test/resources/test_err.jubaql", "--exit-on-failure")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    // check exit code and console output
    exitCode shouldBe 2
    stdoutBuffer.toString should startWith("requestTimeout value is 300000 ms\n")
    stdoutBuffer.toString should endWith("\"error\"\n")
  }

  "Passing a -e option with value" should "print an error and exit" in {
    // start the client
    val command = Seq(startScriptPath, "-f", "src/test/resources/test_err.jubaql", "-e", "value")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    // check exit code and console output
    exitCode shouldBe 1
    stderrBuffer.toString should startWith("Error: Unknown argument 'value'")
  }

  //illegal combination of options(f,e)
  "Passing a -e option without -f" should "print an error and exit" in {
    // start the client
    val command = Seq(startScriptPath, "-e")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    // check exit code and console output
    exitCode shouldBe 1
    stderrBuffer.toString should startWith("Error: -e option requires the -f option")
  }

  "Passing a -c option" should "confirm and execute the commands therein" in {
    val (processIO, stdinBuffer, stdoutBuffer) = getProcessIO()
    stdinBuffer.append("\n")
    stdinBuffer.append("x\n")

    val command = Seq(startScriptPath, "-f", "src/test/resources/test.jubaql", "-c")
    val process = command.run(processIO)
    val exitCode = process.exitValue()

    // check exit code and console output
    exitCode shouldBe 0
    val out = "requestTimeout value is 300000 ms\n" +
              "***(Single step mode: verify command)*******************************************\n" +
              "TRAIN jubatus\n" +
              "***(press return to proceed or enter x and return to cancel)******************** OK\n" +
              "***(Single step mode: verify command)*******************************************\n" +
              "TRAIN jubatus2\n" +
              "***(press return to proceed or enter x and return to cancel)******************** \n"

    stdoutBuffer.toString shouldBe out
  }

  "Passing a --confirmation option" should "confirm and execute the commands therein" in {
    val (processIO, stdinBuffer, stdoutBuffer) = getProcessIO()
    stdinBuffer.append("\n")
    stdinBuffer.append("x\n")

    val command = Seq(startScriptPath, "-f", "src/test/resources/test.jubaql", "--confirmation")
    val process = command.run(processIO)
    val exitCode = process.exitValue()

    // check exit code and console output
    exitCode shouldBe 0
    val out = "requestTimeout value is 300000 ms\n" +
              "***(Single step mode: verify command)*******************************************\n" +
              "TRAIN jubatus\n" +
              "***(press return to proceed or enter x and return to cancel)******************** OK\n" +
              "***(Single step mode: verify command)*******************************************\n" +
              "TRAIN jubatus2\n" +
              "***(press return to proceed or enter x and return to cancel)******************** \n"

    stdoutBuffer.toString shouldBe out
  }

  "Passing a -c option with value" should "print an error and exit" in {
    // start the client
    val command = Seq(startScriptPath, "-f", "src/test/resources/test_err.jubaql", "-c", "value")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    // check exit code and console output
    exitCode shouldBe 1
    stderrBuffer.toString should startWith("Error: Unknown argument 'value'")
  }

  //illegal combination of options(f,c)
  "Passing a -c option without -f" should "print an error and exit" in {
    // start the client
    val command = Seq(startScriptPath, "-c")
    val (logger, stdoutBuffer, stderrBuffer) = getProcessLogger()
    val exitCode = command ! logger
    // check exit code and console output
    exitCode shouldBe 1
    stderrBuffer.toString should startWith("Error: -c option requires the -f option")
  }
}
