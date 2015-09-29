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
import org.json4s.JsonAST.{JNothing, JString}

object JubaQLClient {
  /** Main function to start the JubaQL client shell.
    *
    * Intended usage:
    * `jubaql [-h hostname] [-p portname] [-f scriptfile] [session_id]`
    *
    * The parameters have the following meanings:
    * - `hostname`, `port`: location of the JubaQL gateway
    * - `scriptfile`: if given, execute this file's contents and exit
    * - `session_id`: if given, use this session_id and skip login
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

    // check if scriptfile exists
    scriptfile.foreach(filename => {
      if (!Files.exists(Paths.get(filename))) {
        Console.err.println("Given script file \"%s\" does not exist.".format(filename))
        System.exit(1)
      }
    })

    val serverResponse = sessionIdParameter match {
      case Some(session) => getSessionId(hostname, port, session)
      case _ => getSessionId(hostname, port)
    }
    if (serverResponse.isEmpty) {
      System.exit(1)
    }
    val sessionId = serverResponse.get
    Console.err.println("Using session id \"%s\"".format(sessionId))

    // read from stdin or the given scriptfile
    scriptfile match {
      case Some(filename) =>
        // read lines from file and forward them to server
        for(line <- Source.fromFile(filename).getLines()) {
          if (!line.trim.isEmpty) {
            handleCommand(hostname, port, sessionId, line)
          }
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
          if (line.trim.size > 0 && handleCommand(hostname, port, sessionId, line, output) == false) {
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
  def getSessionId(hostname: String, port: Int,
                   sessionId: String = null, err: PrintStream = Console.err): Option[String] = {
    val url = :/(hostname, port) / "login"
    val req = sessionId match {
      case null => Http(url.POST OK as.String)
      case _ => {
        val payloadData = ("session_id" -> sessionId)
        val json: String = compact(render(payloadData))
        Http(url << json OK as.String)
      }
    }
    req.either.apply() match {
      case Left(error) =>
        // if request fails or is non-2xx, print error
        err.println("[ERROR] " + error.getCause + ": " + error.getMessage)
        None
      case Right(resultJson) =>
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
  }

  /** Sends a JubaQL command to the server and outputs the result.
    * @return false if program should exit, true otherwise
    */
  def handleCommand(hostname: String, port: Int,
                    sessionId: String, cmd: String,
                    out: PrintWriter): Boolean = {
    val url = :/(hostname, port) / "jubaql"
    val payloadData = ("session_id" -> sessionId) ~ ("query" -> cmd)
    val json: String = compact(render(payloadData))
    // create response handler
    val printResponse: Response => Boolean = {
      resp => {
        val resultJson = resp.getResponseBody
        // if we can parse JSON, print it nicely formatted;
        // otherwise print an error
        parseOpt(resultJson) match {
          case Some(result) =>
            if (resp.getStatusCode / 100 != 2) {
              out.print("[ERROR/%s] ".format(resp.getStatusCode))
            }
            result \ "result" match {
              case JString(status) if status == "STATUS" =>
                out.println(status)
                // print all but the "result" field
                val withoutResult = result.removeField(_._1 == "result")
                out.println(pretty(render(withoutResult)))
                true
              case JString(status) =>
                out.println(status)
                out.flush()
                // return false if server shut down successfully
                !status.startsWith("SHUTDOWN")
              case JNothing =>
                out.println("response did not contain a result")
                out.flush()
                true
              case other =>
                out.println(pretty(render(result \ "result")))
                out.flush()
                true
            }
          case None =>
            out.println("[ERROR] failed to parse JSON: \"%s\"".format(resultJson))
            out.flush()
            true
        }
      }
    }
    val req = Http(url << json > printResponse)
    req.either.apply() match {
      case Left(error) =>
        // if request failed, print error
        out.println("[ERROR] " + error.getCause + ": " + error.getMessage)
        out.flush()
        true
      case Right(continue_?) =>
        // if request succeeded, return the result of printResponse
        continue_?
    }
  }

  /** Sends a JubaQL command to the server and outputs the result.
    */
  def handleCommand(hostname: String, port: Int,
                    sessionId: String, cmd: String,
                    out: PrintStream = Console.out): Unit = {
    handleCommand(hostname, port, sessionId, cmd, new PrintWriter(out))
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

      arg[String]("<session_id>") optional() action { (x, o) =>
        o.copy(sessionIdParameter = Some(x))
      } validate { x =>
        if (!x.isEmpty) success else failure("bad session_id")
      } text("use <session_id> and skip login (optional)")
    }

    parser.parse(args, CommandlineOptions())
  }
}

case class CommandlineOptions(hostname: String = CommandlineOptions.defaultHostname,
                              port: Int = CommandlineOptions.defaultPort,
                              scriptfile: Option[String] = None,
                              sessionIdParameter: Option[String] = None)

object CommandlineOptions {
  val defaultHostname = "localhost"
  val defaultPort = 9877
}
