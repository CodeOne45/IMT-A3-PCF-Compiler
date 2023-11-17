package parser
import scala.jdk.CollectionConverters.*
import ast.{AST, Op, Term}
import Term.{App, BOp, Cons, Fix, FixFun, Fun, IfZ, Let, Lit, TList, Var}
import parser.PCFParser

import java.util

class ASTVisitor[AST] extends PCFBaseVisitor[AST] :
  override def visitLit(ctx: PCFParser.LitContext): AST =
    Lit(ctx.getText.toInt).asInstanceOf[AST]

  override def visitBOp(ctx: PCFParser.BOpContext): AST =
    val s = ctx.getChild(1).getText
    val op = Op.parse(s)
    // ctx.term is a Java list, it is translated in a Scala list
    // (initially, to an instance of Buffer, using a collection
    // converter, as Java lists are mutable)
    val ANTLRTerms = ctx.term.asScala.toList
    val List(term1, term2) =
      for (ANTLRTerm <- ANTLRTerms) yield
        visit(ANTLRTerm).asInstanceOf[Term]
    BOp(op, term1, term2).asInstanceOf[AST]

  override def visitIfZ(ctx: PCFParser.IfZContext): AST =
    val ANTLRTerms = ctx.term.asScala.toList
    val List(term1, term2, term3) =
      for (ANTLRTerm <- ANTLRTerms) yield
        visit(ANTLRTerm).asInstanceOf[Term]
    IfZ(term1, term2, term3).asInstanceOf[AST]

  override def visitVar(ctx: PCFParser.VarContext): AST =
      Var(ctx.getText).asInstanceOf[AST]

  override def visitLet(ctx: PCFParser.LetContext): AST =
    val id = ctx.ID.getText
    val ANTLRTerms = ctx.term.asScala.toList
    val List(term1, term2) =
      for (ANTLRTerm <- ANTLRTerms) yield
        visit(ANTLRTerm).asInstanceOf[Term]
    Let(id, term1, term2).asInstanceOf[AST]

  override def visitFun(ctx: PCFParser.FunContext): AST =
    val id = ctx.ID.getText
    val ANTLRTerm = ctx.term
    val term =
        visit(ANTLRTerm).asInstanceOf[Term]
    Fun(id, term).asInstanceOf[AST]

  override def visitApp(ctx: PCFParser.AppContext): AST =
    val ANTLRTerms = ctx.term.asScala.toList
    val List(term1, term2) =
      for (ANTLRTerm <- ANTLRTerms) yield
        visit(ANTLRTerm).asInstanceOf[Term]
    App(term1, term2).asInstanceOf[AST]

  override def visitFix(ctx: PCFParser.FixContext): AST =
    val id = ctx.ID.getText
    val ANTLRTerm = ctx.term
    val term =
      visit(ANTLRTerm).asInstanceOf[Term]
    Fix(id, term).asInstanceOf[AST]

  override def visitFixFun(ctx: PCFParser.FixFunContext): AST =
    val id1 = ctx.ID(0).getText
    val id2 = ctx.ID(1).getText
    val ANTLRTerm = ctx.term
    val term =
      visit(ANTLRTerm).asInstanceOf[Term]
    FixFun(id1, Fun(id2, term)).asInstanceOf[AST]

  override def visitPar(ctx: PCFParser.ParContext): AST =
    visit(ctx.term)

  override def visitNil(ctx: PCFParser.NilContext): AST =
    TList(Nil).asInstanceOf[AST]

  override def visitList(ctx: PCFParser.ListContext): AST =
    var ANTLRTerms = ctx.term.asScala.toList
    val terms = for (ANTLRTerm <- ANTLRTerms) yield
      visit(ANTLRTerm).asInstanceOf[Term]
    TList(terms).asInstanceOf[AST]

  override def visitCons(ctx: PCFParser.ConsContext): AST =
    val term1 = visit(ctx.term(0)).asInstanceOf[Term]
    val term2 = visit(ctx.term(1)).asInstanceOf[Term]
    Cons(term1, term2).asInstanceOf[AST]


