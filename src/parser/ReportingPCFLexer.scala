package parser

import org.antlr.v4.runtime.{CharStream, LexerNoViableAltException}

object Error :
  var flag = false
  var msg = ""

class ReportingPCFLexer(input: CharStream) extends PCFLexer(input) :
  override def recover(e: LexerNoViableAltException): Unit =
    Error.flag = true // report error
    super.recover(e)