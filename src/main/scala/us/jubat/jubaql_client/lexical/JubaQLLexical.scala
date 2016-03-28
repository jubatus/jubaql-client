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

import scala.util.parsing.input.CharArrayReader.EofCh


class JubaQLLexical(keywords: Seq[String]) extends SqlLexical(keywords) {
  case class CodeLit(chars: String) extends Token {
    override def toString = "$$" + chars + "$$"
  }

  // used for parsing $$-delimited code blocks
  protected lazy val codeDelim: Parser[String] = '$' ~ '$' ^^
    { case a ~ b => "$$" }

  protected lazy val stringWithoutCodeDelim: Parser[String] = rep1(chrExcept('$', EofCh)) ^^
    { case chars => chars mkString "" }

  protected lazy val codeContents: Parser[String] = repsep(stringWithoutCodeDelim, '$') ^^
    { case words => words mkString "$" }

  override lazy val token: Parser[Token] =
    (identChar ~ rep(identChar | digit) ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
      | rep1(digit) ~ opt('.' ~> rep(digit)) ^^ {
        case i ~ None => NumericLit(i mkString "")
        case i ~ Some(d) => FloatLit(i.mkString("") + "." + d.mkString(""))
      }
      | '\'' ~ rep(chrExcept('\'', EofCh)) ~ '\'' ^^ { case '\'' ~ chars ~ '\'' => StringLit('\'' + chars.mkString("") + '\'') }
      | '\"' ~ rep(chrExcept('\"', EofCh)) ~ '\"' ^^ { case '\"' ~ chars ~ '\"' => StringLit('\"' + chars.mkString("") + '\"') }
      | codeDelim ~> codeContents <~ codeDelim ^^ { case chars => StringLit(CodeLit(chars).toString()) }
      | EofCh ^^^ EOF
      | codeDelim ~> failure("unclosed code literal")
      | '\'' ~> failure("unclosed string literal")
      | '\"' ~> failure("unclosed string literal")
      | delim
      | failure("illegal character"))
}