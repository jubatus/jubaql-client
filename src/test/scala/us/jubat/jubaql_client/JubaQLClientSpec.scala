// Jubatus: Online machine learning framework for distributed environment
// Copyright (C) 2016 Preferred Networks and Nippon Telegraph and Telephone Corporation.
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
import java.io.{InputStream, ByteArrayOutputStream}
import org.scalatest.PrivateMethodTester._
import scala.util.{Success, Failure}

class JubaQLClientSpec extends FlatSpec with Matchers {
  "splitQueries() for success" should "no comment" in {
    val queryString =
      """
      |SHOW MODELS;
      |CREATE CLASSIFIER MODEL test (label: label) AS name WITH unigram CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}';""".stripMargin

    JubaQLClient.splitQueries(queryString) match {
      case Success(result) =>
        result.size shouldBe 2
        result(0) shouldBe "SHOW MODELS"
        result(1) shouldBe """CREATE CLASSIFIER MODEL test ( label : label ) AS name WITH unigram CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}'"""
      case Failure(e) => fail()
    }
  }

  it should "multiple query for no comment" in {
    val queryString =
      """
      |SHOW MODELS;
      |CREATE CLASSIFIER MODEL test (label: label) AS name WITH unigram
      |CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}';""".stripMargin

    JubaQLClient.splitQueries(queryString) match {
      case Success(result) =>
        result.size shouldBe 2
        result(0) shouldBe "SHOW MODELS"
        result(1) shouldBe """CREATE CLASSIFIER MODEL test ( label : label ) AS name WITH unigram CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}'"""
      case Failure(e) => fail()
    }
  }

  it should "multiple line comment (/* ～ */)" in {
    val queryString =
      """
      |/*
      |SHOW MODELS;
      |*/
      |CREATE CLASSIFIER MODEL test (label: label) AS name WITH unigram
      |CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}';""".stripMargin

    JubaQLClient.splitQueries(queryString) match {
      case Success(result) =>
        result.size shouldBe 1
        result(0) shouldBe """CREATE CLASSIFIER MODEL test ( label : label ) AS name WITH unigram CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}'"""
      case Failure(e) => fail()
    }
  }

  it should "1 line all comment (/* ～ */)" in {
    val queryString =
      """
      |/* SHOW MODELS; */
      |CREATE CLASSIFIER MODEL test (label: label) AS name WITH unigram
      |CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}';""".stripMargin

    JubaQLClient.splitQueries(queryString) match {
      case Success(result) =>
        result.size shouldBe 1
        result(0) shouldBe """CREATE CLASSIFIER MODEL test ( label : label ) AS name WITH unigram CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}'"""
      case Failure(e) => fail()
    }
  }

  it should "1 line all comment (//)" in {
    val queryString =
      """
      |// SHOW MODELS;
      |CREATE CLASSIFIER MODEL test (label: label) AS name WITH unigram
      |CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}';""".stripMargin

    JubaQLClient.splitQueries(queryString) match {
      case Success(result) =>
        result.size shouldBe 1
        result(0) shouldBe """CREATE CLASSIFIER MODEL test ( label : label ) AS name WITH unigram CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}'"""
      case Failure(e) => fail()
    }
  }

  it should "1 line all comment (--)" in {
    val queryString =
      """
      |-- SHOW MODELS;
      |CREATE CLASSIFIER MODEL test (label: label) AS name WITH unigram
      |CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}';""".stripMargin

    JubaQLClient.splitQueries(queryString) match {
      case Success(result) =>
        result.size shouldBe 1
        result(0) shouldBe """CREATE CLASSIFIER MODEL test ( label : label ) AS name WITH unigram CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}'"""
      case Failure(e) => fail()
    }
  }

  it should "1 line all comment (#)" in {
    val queryString =
      """
      |# SHOW MODELS;
      |CREATE CLASSIFIER MODEL test (label: label) AS name WITH unigram
      |CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}';""".stripMargin

    JubaQLClient.splitQueries(queryString) match {
      case Success(result) =>
        result.size shouldBe 1
        result(0) shouldBe """CREATE CLASSIFIER MODEL test ( label : label ) AS name WITH unigram CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}'"""
      case Failure(e) => fail()
    }
  }

  it should "1 line parts comment (/* ～ */)" in {
    val queryString =
      """
      |SHOW MODELS;
      |CREATE CLASSIFIER MODEL test /* come */ (label: label) AS name WITH unigram /* come2 */
      |CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}'; /* come3 */""".stripMargin

    JubaQLClient.splitQueries(queryString) match {
      case Success(result) =>
        result.size shouldBe 2
        result(0) shouldBe "SHOW MODELS"
        result(1) shouldBe """CREATE CLASSIFIER MODEL test ( label : label ) AS name WITH unigram CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}'"""
      case Failure(e) => fail()
    }
  }

  it should "1 line parts comment (//)" in {
    val queryString =
      """
      |SHOW MODELS;
      |CREATE CLASSIFIER MODEL test (label: label) AS name WITH unigram
      |CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}'; // come """.stripMargin

    JubaQLClient.splitQueries(queryString) match {
      case Success(result) =>
        result.size shouldBe 2
        result(0) shouldBe "SHOW MODELS"
        result(1) shouldBe """CREATE CLASSIFIER MODEL test ( label : label ) AS name WITH unigram CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}'"""
      case Failure(e) => fail()
    }
  }

  it should "1 line parts comment (--)" in {
    val queryString =
      """
      |SHOW MODELS;
      |CREATE CLASSIFIER MODEL test (label: label) AS name WITH unigram
      |CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}'; -- come """.stripMargin

    JubaQLClient.splitQueries(queryString) match {
      case Success(result) =>
        result.size shouldBe 2
        result(0) shouldBe "SHOW MODELS"
        result(1) shouldBe """CREATE CLASSIFIER MODEL test ( label : label ) AS name WITH unigram CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}'"""
      case Failure(e) => fail()
    }
  }

  it should "1 line parts comment (#)" in {
    val queryString =
      """
      |SHOW MODELS;
      |CREATE CLASSIFIER MODEL test (label: label) AS name WITH unigram
      |CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}'; # come """.stripMargin

    JubaQLClient.splitQueries(queryString) match {
      case Success(result) =>
        result.size shouldBe 2
        result(0) shouldBe "SHOW MODELS"
        result(1) shouldBe """CREATE CLASSIFIER MODEL test ( label : label ) AS name WITH unigram CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}'"""
      case Failure(e) => fail()
    }
  }

  it should "comment (/* ～ */) no termination" in {
    val queryString =
      """
      |SHOW MODELS;
      |/* CREATE CLASSIFIER MODEL test (label: label) AS name WITH unigram
      |CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}'; """.stripMargin

    JubaQLClient.splitQueries(queryString) match {
      case Success(result) =>
        result.size shouldBe 2
        result(0) shouldBe "SHOW MODELS"
        result(1) shouldBe """/ * CREATE CLASSIFIER MODEL test ( label : label ) AS name WITH unigram CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}'"""
      case Failure(e) => fail()
    }
  }

  it should "comment (/* ～ */) no start" in {
    val queryString =
      """
      |SHOW MODELS;
      |CREATE CLASSIFIER MODEL test (label: label) AS name WITH unigram
      |CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}'; */ """.stripMargin

    JubaQLClient.splitQueries(queryString) match {
      case Success(result) =>
        result.size shouldBe 3
        result(0) shouldBe "SHOW MODELS"
        result(1) shouldBe """CREATE CLASSIFIER MODEL test ( label : label ) AS name WITH unigram CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}'"""
        result(2) shouldBe "* /"
      case Failure(e) => fail()
    }
  }

  "splitQueries() for error" should "query error for single quotes(') no termination" in {
    val queryString =
      """
      |SHOW MODELS;
      |CREATE CLASSIFIER MODEL test (label: label) AS name WITH unigram
      |CONFIG '{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}; # come """.stripMargin

    JubaQLClient.splitQueries(queryString) match {
      case Success(result) => fail()
      case Failure(e) =>
        e.getMessage() should startWith("[ERROR] QueryFile is format error:")
    }
  }

  it should "query error for double quotes(\") no termination" in {
    val queryString =
      """
      |SHOW MODELS;
      |CREATE CLASSIFIER MODEL test (label: label) AS name WITH unigram
      |CONFIG "{"method": "AROW", "parameter": {"regularization_weight" : 1.0}}; # come """.stripMargin

    JubaQLClient.splitQueries(queryString) match {
      case Success(result) => fail()
      case Failure(e) =>
        e.getMessage() should startWith("[ERROR] QueryFile is format error:")
    }
  }

  "confirmQuery()" should "input Enter" in {
    val in = new StandardInputSnatcher()
    val out = new ByteArrayOutputStream()
    val orgIn = Console.in
    val orgOut = Console.out
    Console.setIn(in)
    Console.setOut(out)

    in.inputin("")
    val result = JubaQLClient.confirmQuery("SHOW MODEL")

    val outResult = "***(Single step mode: verify command)*******************************************\n" +
                    "SHOW MODEL\n" +
                    "***(press return to proceed or enter x and return to cancel)******************** "
    result shouldBe true
    out.toString() shouldBe outResult

    Console.setIn(orgIn)
    Console.setOut(orgOut)
  }

  it should "input x + Enter" in {
    val in = new StandardInputSnatcher()
    val out = new ByteArrayOutputStream()
    val orgIn = Console.in
    val orgOut = Console.out
    Console.setIn(in)
    Console.setOut(out)

    in.inputin("x")
    val result = JubaQLClient.confirmQuery("SHOW MODEL")

    val outResult = "***(Single step mode: verify command)*******************************************\n" +
                    "SHOW MODEL\n" +
                    "***(press return to proceed or enter x and return to cancel)******************** "
    result shouldBe false
    out.toString() shouldBe outResult

    Console.setIn(orgIn)
    Console.setOut(orgOut)
  }

  it should "input other + Enter" in {
    val in = new StandardInputSnatcher()
    val out = new ByteArrayOutputStream()
    val orgIn = Console.in
    val orgOut = Console.out
    Console.setIn(in)
    Console.setOut(out)

    in.inputin("a")
    in.inputin("b")
    in.inputin("")
    val result = JubaQLClient.confirmQuery("SHOW MODEL")

    val outResult = "***(Single step mode: verify command)*******************************************\n" +
                    "SHOW MODEL\n" +
                    "***(press return to proceed or enter x and return to cancel)******************** " +
                    "***(press return to proceed or enter x and return to cancel)******************** " +
                    "***(press return to proceed or enter x and return to cancel)******************** "
    result shouldBe true
    out.toString() shouldBe outResult

    Console.setIn(orgIn)
    Console.setOut(orgOut)
  }
}

class StandardInputSnatcher extends InputStream {
  private val buffer = new StringBuilder()
  private val crlf = "\n"

  def inputin(str: String): Unit = {
    buffer.append(str).append(crlf)
  }

  override def read(): Int = {
    var result = -1
    if (buffer.isDefinedAt(0)) {
      result = buffer.charAt(0)
      buffer.deleteCharAt(0)
    }
    result
   }
}