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
package us.jubat.jubaql_client.lexical

import org.scalatest._

class JubaQLLexicalSpec extends FlatSpec with Matchers {

  private def getTokenList(text: String): List[String] = {
    val lexical = new JubaQLLexical(Seq())
    var scanner = new lexical.Scanner(text)
    var tokenList = List[String]()
    while (!scanner.atEnd) {
      tokenList = tokenList :+ scanner.first.chars
      scanner = scanner.rest
    }
    tokenList
  }

  "A JubaQLLexical for single token" should "string" in {
    val parsText = "test"
    getTokenList(parsText) shouldBe List("test")
  }

  it should "numericlit" in {
    val parsText = "1234"
    getTokenList(parsText) shouldBe List("1234")
  }

  it should "floatlit" in {
    val parsText = "1.0"
    getTokenList(parsText) shouldBe List("1.0")
  }

  it should "stringlit for single quotes(')" in {
    val parsText = """'test'"""
    getTokenList(parsText) shouldBe List("""'test'""")
  }

  it should "stringlit for double quotes(\")" in {
    val parsText = """"test""""
    getTokenList(parsText) shouldBe List(""""test"""")
  }

  it should "code delimited($$)" in {
    val parsText = "$$ test $$"
    getTokenList(parsText) shouldBe List("$$ test $$")
  }

  "A JubaQLLexical for multiple token" should "string and numericli and floatlit" in {
    val parsText = "test 1234 9.99"
    getTokenList(parsText) shouldBe List("test", "1234", "9.99")
  }

  it should "string and stringlit for single quotes(')" in {
    val parsText = """test '{1234, 9.99}'"""
    getTokenList(parsText) shouldBe List("test", """'{1234, 9.99}'""")
  }

  it should "string and stringlit for double quotes(\")" in {
    val parsText = """test "{1234, 9.99}""""
    getTokenList(parsText) shouldBe List("test", """"{1234, 9.99}"""")
  }

  it should "string and delimited" in {
    val parsText = "test $$ test2 $$"
    getTokenList(parsText) shouldBe List("test", "$$ test2 $$")
  }

  it should "stringlit for single quotes(') and double quotes(\")" in {
    val parsText = """'{"1234", "9.99"}'"""
    getTokenList(parsText) shouldBe List("""'{"1234", "9.99"}'""")
  }

  it should "stringlit for double quotes(\") and single quotes(')" in {
    val parsText = """"{'1234', '9.99'}""""
    getTokenList(parsText) shouldBe List(""""{'1234', '9.99'}"""")
  }

  "A JubaQLLexical for comment" should "All comments (/* ～ */)" in {
    val parsText = "/* test test2 */"
    getTokenList(parsText).isEmpty shouldBe true
  }

  it should "All comments (/* ～ */) line break" in {
    val parsText = "/* test \ntest2 */"
    getTokenList(parsText).isEmpty shouldBe true
  }

  it should "All comments (//)" in {
    val parsText = "// test test2"
    getTokenList(parsText).isEmpty shouldBe true
  }

  it should "All comments (--)" in {
    val parsText = "-- test test2"
    getTokenList(parsText).isEmpty shouldBe true
  }

  it should "All comments (#)" in {
    val parsText = "# test test2"
    getTokenList(parsText).isEmpty shouldBe true
  }

  it should "part comments (/* ～ */)" in {
    val parsText = "test /* test2 */ test3"
    getTokenList(parsText) shouldBe List("test", "test3")
  }

  it should "part comments (//)" in {
    val parsText = "test test2 // test3"
    getTokenList(parsText) shouldBe List("test", "test2")
  }

  it should "part comments (--)" in {
    val parsText = "test test2 -- test3"
    getTokenList(parsText) shouldBe List("test", "test2")
  }

  it should "part comments (#)" in {
    val parsText = "test test2 # test3"
    getTokenList(parsText) shouldBe List("test", "test2")
  }

  it should "comment(/* ～ */) no termination" in {
    val parsText = "/* test test2"
    getTokenList(parsText) shouldBe List("/", "*", "test", "test2")
  }

  it should "comment(/* ～ */) no start" in {
    val parsText = "test test2 */"
    getTokenList(parsText) shouldBe List("test", "test2", "*", "/")
  }

  "A JubaQLLexical for error" should "stringlit for single quotes(') no termination" in {
    val parsText = """'test"""
    getTokenList(parsText).mkString(" ") should include("error")
  }

  it should "stringlit for double quotes(\") no termination" in {
    val parsText = """"test"""
    getTokenList(parsText).mkString(" ") should include("error")
  }

  it should "code delimited($$) no termination" in {
    val parsText = "$$ test"
    getTokenList(parsText).mkString(" ") should include("error")
  }

  it should "stringlit for single quotes(') no start" in {
    val parsText = """test'"""
    getTokenList(parsText).mkString(" ") should include("error")
  }

  it should "stringlit for double quotes(\") no start" in {
    val parsText = """test""""
    getTokenList(parsText).mkString(" ") should include("error")
  }

  it should "code delimited($$) no start" in {
    val parsText = "test $$"
    getTokenList(parsText).mkString(" ") should include("error")
  }
}