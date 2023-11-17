package pcf

import java.io.{FileInputStream, FileWriter}
import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}
import parser.{ASTVisitor, Error, ErrorListener, PCFParser, ReportingPCFLexer, SyntaxError}
import ast.Term
import interp.Value
import interp.Value.BuiltIn
import typer.{Typer, Type}

object Main :
  def main(args: Array[String]): Unit =
    val (is, filename) =
      if args.length == 0
      then (System.in, None)
      else (new FileInputStream(args(0)), Some(args(0)))
    val verbose = args.length == 0 || args.length > 1 && args(1) == "-v"
    if (args.contains("-i")) println(s"==> ${interpret()}")
    else compile()

    def analyze(): (Term, Type) =
      val input = new ANTLRInputStream(is)
      // val lexer = new CalcLexer(input)g
      val lexer = new ReportingPCFLexer(input)
      val tokens = new CommonTokenStream(lexer)
      val parser = new PCFParser(tokens)
      parser.removeErrorListeners()
      parser.addErrorListener(new ErrorListener())
      val tree = parser.term()
      if (verbose) println(s"ANTLR Parse Tree: ${tree.toStringTree(parser)}")
      if ! Error.flag then
        val visitor = new ASTVisitor
        val term = visitor.visit(tree).asInstanceOf[Term]
        if (verbose) println(s"AST: $term")
        val a = Typer.typer(term, Typer.env0)
        (term, a)
      else throw new SyntaxError(Error.msg)

    def interpret(): String =
      val (term, a) = analyze()
      val value = interp.Interp.interp(term, Map("head" -> BuiltIn("head"),
        "tail" -> BuiltIn("tail"),
        "is_empty" -> BuiltIn("is_empty")))
      s"$value:$a"

    def compile(): Unit =
      val (term, _) = analyze()
      val aterm = term.annotate(Nil)
      if (verbose) println(s"annotated AST: $aterm")
      val code = gen.Gen.gen(aterm)
      if (filename.isDefined) write(code)
      else println(code)

    // write code to .wat file associated to .pcf file passed as argument,
    // returning .wat file relative filename
    def write(code: String): String =
      val CFilename = filename.get.replaceFirst("\\.pcf\\z", ".wat")
      if (verbose) System.out.println("writing .wat code to " + CFilename)
      val out = new FileWriter(CFilename)
      out.write(code)
      out.flush()
      out.close()
      CFilename