package gen

import ast.{ATerm, Op}
import ast.ATerm.*
import ast.Op.*
import gen.Code.*

object Gen:

  // push $ENV on the stack
  val PushEnv: Code =
    Seq(List(
      Ins(";; PushEnv"), // comment
      Ins("global.get $ENV"),
    ))

  // pop $ENV from the stack
  val PopEnv: Code =
    val comment = Ins(";; PopEnv")
    val ins1 = Ins("(global.set $ACC)")
    val ins2 = Ins("(global.set $ENV)")
    val ins3 = Ins("(global.get $ACC)")
    Seq(List(comment, ins1, ins2, ins3))

  // retrieve value of variable with de Bruijn index idx
  private def Search(idx: Int): Code =
    val ins1 = Ins(s"(i32.const $idx)")
    val ins2 = Ins("(global.get $ENV)")
    val ins3 = Ins("(call $search)")
    Seq(List(ins1, ins2, ins3))


  // extend the environment with the value on top of the stack
  val Extend: Code =
    val ins1 = Ins("(global.get $ENV)")
    val ins2 = Ins("(call $cons)")
    val ins3 = Ins("(global.set $ENV)")
    Seq(List(ins1, ins2, ins3))

  private def MkClos(idx: Int): Code =
    val ins0 = Ins(";; MkClos")
    val ins1 = Ins(s"(i32.const $idx)")
    val ins2 = Ins("(global.get $ENV)")
    val ins3 = Ins("(call $pair)")
    Seq(List(ins0, ins1, ins2, ins3))

  private val Apply : Code = Ins("(call $apply)")

  private var idx = 0 // next index in the table of closure bodies
  private var bodies: List[Code] = Nil // list of closure bodies

  // initialize the table of closure bodies
  private def initTable(): Unit = {
    idx = 0
    bodies = Nil
  }

  //  Prelude had some errors when importing file
  //  def prelude(): String =
  //    val source = Source.fromFile("pcf/prelude.wat")
  //    val contents = source.mkString
  //    source.close()
  //    contents

  def gen(aterm: ATerm): String =
    initTable() // initialize variables before each test

    val prelude =
      """(module
        |  (memory 1 10)
        |  (global $HEAP (mut i32) (i32.const 0)) ;; heap pointer initialized to 0
        |  (global $ENV  (mut i32) (i32.const 0)) ;; env pointer initialized to NIL
        |  (global $ACC  (mut i32) (i32.const 999)) ;; accumulator initialized to 999
        |  (global $LIST i32 (i32.const 1))       ;; LIST tag (for non empty lists)
        |  (global $NIL  i32 (i32.const 0))       ;; NIL tag (for empty lists)
        |  (func $pair (param $first i32) (param $second i32) (result i32)
        |    (local $result i32)
        |    (local.set $result (global.get $HEAP))
        |    (i32.store (global.get $HEAP) (local.get $first))
        |    (global.set $HEAP (i32.add (global.get $HEAP) (i32.const 4)))
        |    (i32.store (global.get $HEAP) (local.get $second))
        |    (global.set $HEAP (i32.add (global.get $HEAP) (i32.const 4)))
        |    (local.get $result)
        |    return)
        |  (func $head (param $list i32) (result i32)
        |    (i32.load (i32.add (local.get $list (i32.const 4))))
        |    return)
        |  (func $tail (param $list i32) (result i32)
        |    (i32.load (i32.add (local.get $list (i32.const 8))))
        |    return)
        |  ;; a cons is stored as a block of 3 words: a LIST tag, the head and the tail
        |  (func $cons (param $head i32) (param $tail i32) (result i32)
        |    (local $result i32)
        |    (local.set $result (global.get $HEAP))
        |    (i32.store (global.get $HEAP) (global.get $LIST))
        |    (global.set $HEAP (i32.add (global.get $HEAP) (i32.const 4)))
        |    (i32.store (global.get $HEAP) (local.get $head))
        |    (global.set $HEAP (i32.add (global.get $HEAP) (i32.const 4)))
        |    (i32.store (global.get $HEAP) (local.get $tail))
        |    (global.set $HEAP (i32.add (global.get $HEAP) (i32.const 4)))
        |    (local.get $result)
        |    return)
        |    ;; retrieves the element $n of the list $list (starting from 0)
        |  ;; precondition: the size of the list is greater than $n
        |  (func $search (param $n i32) (param $list i32) (result i32)
        |    (local.get $n)
        |    (if (result i32)
        |      (then            ;; n is non zero
        |       (i32.sub (local.get $n) (i32.const 1))
        |       (local.get $list)
        |       (call $tail)
        |       (call $search))
        |      (else            ;; n is zero
        |       (local.get $list)
        |       (call $head)))
        |    return)
        |  (func $apply (param $W i32)(param $C i32)(result i32)
        |      (local $e i32) ;; the environment e stored in the closure
        |      (local.get $W) ;; element 0 of the environment
        |      ;;(local.get $C) ;; element 1 of the environment
        |      ;; check if the closure is reflexive"),
        |    ;; retrieve the environment in the closure (2nd element of a pair)
        |      (local.set $e (i32.load (i32.add (local.get $C)(i32.const 4))))
        |    ;; extend the environment e to <W, <C, e>>
        |      (local.get $e)
        |      ;;(call $cons)
        |      (call $cons)
        |      (global.set $ENV)
        |    ;; retrieve index of closure body and executes the body
        |    (call_indirect (result i32) (i32.load (local.get $C)))
        |  )
        |    """.stripMargin // removes whitespaces

    val postlude = ")\n"

    val body =
      """  (func (export "main") (result i32)
        |  """.stripMargin +
        format(2, emit(aterm)) +
        "    return)\n"

    prelude + emitTable + emitFunctions + body + postlude

  // add closure body to the list
  private def addBody(body: Code): Unit =
    bodies = bodies :+ body

  private def emit(term: ATerm): Code = term match
    case Lit(n) => Ins(s"i32.const $n")
    case BOp(op: Op, t1, t2) => Seq(List(emit(t1), emit(t2), emitOp(op)))
    case IfZ(cond, t1, t2) =>
      val c1 = emit(t1)
      val c2 = emit(t2)
      val c3 = emit(cond)
      Seq(List(c3, Test(c2, c1)))
    
    case Var(id, idx) =>
      val c = Search(idx)
      Seq(List(Ins(s";; Var"), c))

    case Let(id, t1, t2) =>
      val c1 = emit(t1)
      val c2 = emit(t2)
      Seq(List(Ins(";; Let"), PushEnv, c1, Extend, c2, PopEnv))

    case Fun(id, t) =>
      val c = emit(t)
      addBody(c) // add closure body to the list
      val code = Seq(List(Ins(";; Fun"), MkClos(idx)))
      idx += 1 // increment index in the table of closure bodies
      code

    // We must calculate the index of the name f of the recursive function.
    // In the case of a non-recursive function, this name does not appear in t
    // and keeping the value of f in the environment is useless,
    // but there will still be an entry in the environment created by
    // the Apply statement.
    case FixFun(id1, Fun(id2, t)) =>
      val idx = this.idx // save the current index
      val c = emit(Fun(id2, t))
      val code = Seq(List(Ins(";; FixFun"), MkClos(idx)))
      code

    case App(t1, t2) =>
      val c1 = emit(t1)
      val c2 = emit(t2)
      Seq(List(Ins(";; App"), PushEnv, c2, c1, Apply, PopEnv))


    case _ => ??? // should never happen

  private def spaces(depth: Int): String =
    (for i <- 0 until depth yield " ").mkString

  private def format(d: Int, code: Code): String = code match {
    case Ins(s) => s"${spaces(d)}$s\n"
    case Seq(s) => (for (c <- s) yield format(d, c)).mkString("\n")
    case Test(b1, b2) => s"""${spaces(d)}(if (result i32)
                            |${spaces(d)}(then
                            |${format(d + 2, b1)}
                            |${spaces(d)})
                            |${spaces(d)}(else
                            |${format(d + 2, b2)}
                            |${spaces(d)})
                            |${spaces(d)})
                            |""".stripMargin
  }

  def emitOp(op: Op): Code = op match
    case PLUS => Ins("i32.add")
    case MINUS => Ins("i32.sub")
    case TIMES => Ins("i32.mul")
    case DIVIDE => Ins("i32.div_s") // signed division
    case _ => ???


  // generate table of function references
  private def emitTable: String =
    val table = bodies.zipWithIndex.map((_, i) => s"$$closure$i")
    s"""(type (;0;) (func (param i32) (result i32)))
       |(table funcref
       |  (elem
       |    ${table.mkString(" ")}
       |  )
       |)""".stripMargin


  // generate functions for closure bodies
  private def emitFunctions: String =
    val functions = bodies.zipWithIndex.map((c, i) => emitFunction(i, c))
    functions.mkString("\n")

  // generate function for closure body where the name is the functionName(i) name of the function
  private def emitFunction(i: Int, body: Code): String =
    s"""(func ${functionName(i)} (result i32)
       |${format(2, body)}
       |  return)""".stripMargin
  // generate name of function for closure body
  private def functionName(i: Int) = "$closure" + i

