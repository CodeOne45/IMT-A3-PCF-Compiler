package ast

trait AST

enum ATerm extends AST :
  case Lit(n: Int)
  case BOp(op: Op, t1: ATerm, t2: ATerm)
  case IfZ(t1: ATerm, t2: ATerm, t3: ATerm)
  case Var(id: String, idx: Int)
  case Let(id: String, t1: ATerm, t2: ATerm)
  case Fun(id: String, t: ATerm)
  case App(t1: ATerm, t2: ATerm)
  case Fix(id: String, t: ATerm)
  case FixFun(id: String, t: ATerm)
  case TList(ts: List[ATerm])
  case Cons(t1: ATerm, t2: ATerm)

enum Term extends AST :
  case Lit(n: Int)
  case BOp(op: Op, t1: Term, t2: Term)
  case IfZ(t1: Term, t2: Term, t3: Term)
  case Var(id: String)
  case Let(id: String, t1: Term, t2: Term)
  case Fun(id: String, t: Term)
  case App(t1: Term, t2: Term)
  case Fix(id: String, t: Term)
  case FixFun(id: String, t: Term)
  case TList(ts: List[Term])
  case Cons(t1: Term, t2: Term)

  def annotate(e: List[String]): ATerm = this match
    case Lit(n) => ATerm.Lit(n)
    case Var(id: String) => ATerm.Var(id, e.indexOf(id))
    case BOp(op, t1, t2) => ATerm.BOp(op, t1.annotate(e), t2.annotate(e))
    case IfZ(cond, t2, t3) => ATerm.IfZ(cond.annotate(e), t2.annotate(e), t3.annotate(e))
    case Let(id, t1, t2) => ATerm.Let(id, t1.annotate(e), t2.annotate(id :: e))
    case Fun(id, t) => ATerm.Fun(id, t.annotate(id :: e))
    case FixFun(id, t) => ATerm.FixFun(id, t.annotate(id :: e))
    case App(t1, t2) => ATerm.App(t1.annotate(e), t2.annotate(e))
    case Fix(id, t) => ATerm.Fix(id, t.annotate(id :: e))
    case TList(ts) => ATerm.TList(ts.map(_.annotate(e)))
    case Cons(t1, t2) => ATerm.Cons(t1.annotate(e), t2.annotate(e))



enum Op :
  case PLUS, MINUS, TIMES, DIVIDE

object Op :
  def parse(s: String): Op = s match
    case "+" => PLUS
    case "-" => MINUS
    case "*" => TIMES
    case "/" => DIVIDE
    case _ => ??? // should never happen
