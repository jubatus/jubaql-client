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

import com.ning.http.client.Response
import dispatch._
import dispatch.Defaults._
import scopt.OptionParser
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._
import java.io.{PrintStream, PrintWriter}
import jline.console.ConsoleReader
import java.nio.file.{Paths, Files}
import scala.io.Source
import org.json4s.JsonAST.{JNothing, JString, JBool, JObject}
import us.jubat.jubaql_client.lexical._
import scala.util.{Try, Success, Failure}

object JubaQLClient {
  /** Main function to start the JubaQL client shell.
    *
    * Intended usage:
    * `jubaql [-h hostname] [-p port] [-f scriptfile [-e] [-c]] [-t timeout] [-s session_id [-r]]`
    *
    * The parameters have the following meanings:
    * - `hostname`, `port`: location of the JubaQL gateway
    * - `scriptfile`: if given, execute this file's contents and exit
    * - `timeout`: request timeout value
    * - `session_id`: if given, to connect to the JubaQL gateway using this session_id
    *   (otherwise, login will be done on program start)
    */
  def main(args: Array[String]) {
    val maybeParsedOptions: Option[CommandlineOptions] = parseCommandlineOption(args)
    if (maybeParsedOptions.isEmpty)
      System.exit(1)
    val parsedOptions = maybeParsedOptions.get
    val hostname: String = parsedOptions.hostname
    val port: Int = parsedOptions.port
    val scriptfile: Option[String] = parsedOptions.scriptfile
    val sessionIdParameter: Option[String] = parsedOptions.sessionIdParameter
    val timeout = parsedOptions.timeout.fold(CommandlineOptions.defaultTimeout * 1000) { value =>
      if (value == 0) { -1 } else { value * 1000}
    }
    Console.out.println(s"requestTimeout value is ${timeout} ms")
    val isReconnect: Boolean = parsedOptions.reconnect

    // check if scriptfile exists
    scriptfile.foreach(filename => {
      if (!Files.exists(Paths.get(filename))) {
        Console.err.println("Given script file \"%s\" does not exist.".format(filename))
        System.exit(1)
      }
    })

    val serverResponse = getSessionId(hostname, port, timeout, sessionIdParameter, isReconnect)

    if (serverResponse.isEmpty) {
      System.exit(1)
    }
    val sessionId = serverResponse.get
    Console.err.println("Using session id \"%s\"".format(sessionId))

    // read from stdin or the given scriptfile
    scriptfile match {
      case Some(filename) =>
        // read lines from file and forward them to server
        loadQueriesFromFile(filename) match {
          case Success(queryList) =>
            queryList.foreach { query =>
              if (confirmQuery(query, parsedOptions.confirmation)) {
                handleCommand(hostname, port, timeout, sessionId, query) match {
                  case ResultType.Error if (parsedOptions.exitOnFailure) =>
                    System.exit(2)

                  case _ =>
                }
              }
            }
          case Failure(e) =>
            Console.err.println(e.getMessage())
            System.exit(2)
        }
      case None =>
        // ask for commands and forward them to server
        Console.err.println("Please enter your JubaQL commands. Type Ctrl+D or \"exit\" to quit.")
        val reader = new ConsoleReader()
        reader.setExpandEvents(false)
        val output = new PrintWriter(reader.getOutput)
        reader.setPrompt("jubaql> ")
        var line = reader.readLine()
        while (line != null && line != "exit") {
          if (line.trim.size > 0 && handleCommand(hostname, port, timeout, sessionId, line, output) == ResultType.Exit) {
            line = null
          } else {
            line = reader.readLine()
          }
        }
        // write one more line to get the next console prompt right
        if (line == null)
          println("")
    }

    // explicitly exiting seems to be necessary in some cases,
    // or the application will hang here
    System.exit(0)
  }

  /**
   * Gets a session id from a JubaQL gateway server.
   *
   * @return some session id if login was successful
   */
  def getSessionId(hostname: String, port: Int, timeout: Int,
                   sessionId: Option[String], isReconnect: Boolean, err: PrintStream = Console.err): Option[String] = {
    val url = :/(hostname, port) / "login"
    val myHttp = Http.configure(_.setRequestTimeoutInMs(timeout).setIdleConnectionTimeoutInMs(timeout))
    val payloadData = (sessionId, isReconnect) match {
      case (Some(id), true) =>
        //connect session
        ("new" -> false) ~ ("session_id" -> id)
      case (Some(id), false) =>
        // To create session, using the session id.
        ("new" -> true) ~ ("session_id" -> id)
      case (None, false) =>
        // To create session, session id to be generated in JubaQL Gateway.
        JObject("new" -> JBool(true))
      case _ =>
        err.println(s"[ERROR] Internal Error. illegal argument: sessionId = ${sessionId}, reconnect flag = ${isReconnect}")
        return None
    }

    val req = myHttp(url << compact(render(payloadData)))

    req.either.apply() match {
      case Right(resp) =>
        if (resp.getStatusCode / 100 != 2) {
          err.println("[ERROR/%s] %s".format(resp.getStatusCode, resp.getResponseBody))
          None
        } else {
          val resultJson = resp.getResponseBody
          parseOpt(resultJson) match {
            case Some(result) =>
              (result \ "session_id") match {
                case JString(session_id) =>
                  Some(session_id)
                case _ =>
                  err.println("[ERROR] JSON did not contain session_id")
                  None
              }
            case None =>
              err.println("[ERROR] failed to parse JSON: \"%s\"".format(resultJson))
              None
          }
        }
      case Left(error) =>
        // request fails with network error
        if (error.getCause() != null) {
          err.println(s"[ERROR] ${error.getCause()}: ${error.getMessage()}")
        } else {
          err.println(s"[ERROR] ${error.toString()}")
        }
        None
    }
  }

  /** Sends a JubaQL command to the server and outputs the result.
    * @return false if program should exit, true otherwise
    */
  def handleCommand(hostname: String, port: Int, timeout: Int,
                    sessionId: String, cmd: String,
                    out: PrintWriter): ResultType = {
    val url = :/(hostname, port) / "jubaql"
    val payloadData = ("session_id" -> sessionId) ~ ("query" -> cmd)
    val json: String = compact(render(payloadData))
    // create response handler
    val printResponse: Response => ResultType = {
      resp => {
        val resultJson = resp.getResponseBody
        // if we can parse JSON, print it nicely formatted;
        // otherwise print an error
        parseOpt(resultJson) match {
          case Some(result) =>
            var resultType: Option[ResultType] = None
            if (resp.getStatusCode / 100 != 2) {
              out.print("[ERROR/%s] %s".format(resp.getStatusCode, resp.getResponseBody))
              resultType = Some(ResultType.Error)
            }
            result \ "result" match {
              case JString(status) if status == "STATUS" =>
                out.println(status)
                // print all but the "result" field
                val withoutResult = result.removeField(_._1 == "result")
                out.println(pretty(render(withoutResult)))
                out.flush()
                if (resultType.isEmpty) resultType = Some(ResultType.Success)
              case JString(status) =>
                out.println(status)
                out.flush()
                // return false if server shut down successfully
                if (status.startsWith("SHUTDOWN")) resultType = Some(ResultType.Exit)
                else if (resultType.isEmpty) resultType = Some(ResultType.Success)
              case JNothing =>
                out.println("response did not contain a result")
                out.flush()
                resultType = Some(ResultType.Error)
              case other =>
                out.println(pretty(render(result \ "result")))
                out.flush()
                if (resultType.isEmpty) resultType = Some(ResultType.Success)
            }
            resultType.get
          case None =>
            out.println("[ERROR] failed to parse JSON: \"%s\"".format(resultJson))
            out.flush()
            ResultType.Error
        }
      }
    }

    val myHttp = Http.configure(_.setRequestTimeoutInMs(timeout).setIdleConnectionTimeoutInMs(timeout))
    val req = myHttp(url << json > printResponse)
    req.either.apply() match {
      case Left(error) =>
        // if request failed, print error
        if (error.getCause() != null) {
          out.println(s"[ERROR] ${error.getCause()}: ${error.getMessage()}")
        } else {
          out.println(s"[ERROR] ${error.toString()}")
        }
        out.flush()
        ResultType.Error
      case Right(continue_?) =>
        // if request succeeded, return the result of printResponse
        continue_?
    }
  }

  /** Sends a JubaQL command to the server and outputs the result.
    */
  def handleCommand(hostname: String, port: Int, timeout: Int,
                    sessionId: String, cmd: String,
                    out: PrintStream = Console.out): ResultType = {
    handleCommand(hostname, port, timeout, sessionId, cmd, new PrintWriter(out))
  }

  /** Returns parsed commandline option.
    *
    * Specifications of options are written at the main function.
    *
    * @param args
    * @return Returns parsed options if successfully parsed.
    *         Otherwise, returns None.
    */
  def parseCommandlineOption(args: Array[String]): Option[CommandlineOptions] = {
    val parser = new OptionParser[CommandlineOptions]("JubaQLClient") {
      opt[String]('h', "hostname") optional() valueName("<hostname>") action { (x, o) =>
        o.copy(hostname = x)
      } validate { x =>
        if (!x.isEmpty) success else failure("hostname must not be empty string")
      } text("hostname of the JubaQL gateway (default: " + CommandlineOptions.defaultHostname + ")")

      opt[Int]('p', "port") optional() valueName("<port>") action { (x, o) =>
        o.copy(port = x)
      } validate { x =>
        if (x >= 1 && x <= 65535) success else failure("bad port number; port number n must be \"1 <= n <= 65535\"")
      } text("port of the JubaQL gateway (default: " + CommandlineOptions.defaultPort + ")")

      opt[String]('f', "scriptfile") optional() valueName("<scriptfile>") action { (x, o) =>
        o.copy(scriptfile = Some(x))
      } validate { x =>
        if (!x.isEmpty) success else failure("bad scriptfile name")
      } text("execute <scriptfile> contents (optional)")

      opt[Int]('t', "timeout") optional() valueName("<timeout>") action { (x, o) =>
        o.copy(timeout = Some(x))
      } validate { x =>
        if (x >= 0 && x <= 2145600) success else failure("bad timeout value; timeout value n must be \"0 <= n <= 2145600\"")
      } text(s"request timeout value (default: ${CommandlineOptions.defaultTimeout}sec)")

      opt[String]('s', "session") optional() valueName("<session_id>") action { (x, o) =>
        o.copy(sessionIdParameter = Some(x))
      } validate { x =>
        if (!x.isEmpty) success else failure("bad session_id")
      } text("connect to the JubaQL gateway using <session_id> (optional)")

      opt[Unit]('r', "reconnect") optional() valueName("<session_id>") action { (x, o) =>
        o.copy(reconnect = true)
      } text("reconnect flag (optional)")

      opt[Unit]('e', "exit-on-failure") optional() action { (x, o) =>
        o.copy(exitOnFailure = true)
      } text("exit-on-failure flag (optional)")

      opt[Unit]('c', "confirmation") optional() action { (x, o) =>
        o.copy(confirmation = true)
      } text("confirmation flag (optional)")
    }
    parser.checkConfig { x =>
      if (x.sessionIdParameter.isEmpty && x.reconnect) {
        Left("-r option requires the -s option")
      } else if (x.scriptfile.isEmpty && x.exitOnFailure) {
        Left("-e option requires the -f option")
      } else if (x.scriptfile.isEmpty && x.confirmation) {
        Left("-c option requires the -f option")
      } else {
        Right()
      }
    }

    parser.parse(args, CommandlineOptions())
  }

  private def loadQueriesFromFile(filename: String): Try[List[String]] = {
    splitQueries(Source.fromFile(filename).mkString)
  }

  private[jubaql_client] def splitQueries(fileContents: String): Try[List[String]] = Try {
    val lexical = new JubaQLLexical(Seq())
    var scanner = new lexical.Scanner(fileContents)
    var tokenList = List[String]()
    var queryList = List[String]()
    while (!scanner.atEnd) {
      if (scanner.first.isInstanceOf[lexical.ErrorToken]) {
        throw new Exception(s"[ERROR] QueryFile is format error: ${scanner.first.chars}")
      }
      if (scanner.first.chars == ";") {
        queryList = queryList :+ tokenList.mkString(" ")
        tokenList = List.empty
      } else {
        tokenList = tokenList :+ scanner.first.chars
      }
      scanner = scanner.rest
    }

    if (tokenList.nonEmpty) {
      queryList = queryList :+ tokenList.mkString(" ")
    }
    queryList
  }

  private[jubaql_client] def confirmQuery(query: String, isConfirm: Boolean): Boolean = {
    if (isConfirm) {
      confirmQuery(query)
    } else {
      true
    }
  }

  private[jubaql_client] def confirmQuery(query: String): Boolean = {
    Console.out.println("***(Single step mode: verify command)*******************************************")
    Console.out.println(query)

    var result = false
    var retry = true
    while (retry) {
      val input = readLine("***(press return to proceed or enter x and return to cancel)******************** ")
      retry = false
      if (input.isEmpty) result = true  // proceed
      else if (input == "x") result = false  // cancelled
      else retry = true
    }
    result
  }
}

case class CommandlineOptions(hostname: String = CommandlineOptions.defaultHostname,
                              port: Int = CommandlineOptions.defaultPort,
                              scriptfile: Option[String] = None,
                              timeout: Option[Int] = None,
                              sessionIdParameter: Option[String] = None,
                              reconnect: Boolean = false,
                              exitOnFailure: Boolean = false,
                              confirmation: Boolean = false)

object CommandlineOptions {
  val defaultHostname = "localhost"
  val defaultPort = 9877
  val defaultTimeout = 300
}

sealed abstract class ResultType(aName: String) {
  val name = aName
}

object ResultType {
  case object Success extends ResultType("Success")
  case object Error extends ResultType("Error")
  case object Exit extends ResultType("Exit")
}
