package interp

import ast.Term
import Term.*
import ast.Op.*
import Value.*

import scala.language.implicitConversions

object Interp :
  type Env = Map[String, Value | IceCube]
  implicit def Int2Value(i: Int): Value = IntVal(i)
  implicit def Value2Int(v: Value): Int =
    v match
      case IntVal(i) => i
      case _ => throw new InterpError(s"expecting integer, found $v")

  def interp(t: Term, e: Env): Value = t match
    case Lit(value) => value
    case BOp(op, t1, t2) =>
      val v1 = interp(t1, e)
      val v2 = interp(t2, e)
      op match
        case PLUS => v1 + v2
        case MINUS => v1 - v2
        case TIMES => v1 * v2
        case DIVIDE => v1 / v2
    case IfZ(test, t1, t2) =>
      if interp(test, e) == IntVal(0) then interp(t1, e)
      else interp(t2, e)
    case Var(x) =>
      e.get(x) match
        case Some(IceCube(x1, t1, e1)) => interp(Fix(x1, t1), e1)
        case Some(value: Value) => value
        case None => throw new InterpError(s"undefined variable $x in $e")
    case Let(x, t1, t2) =>
      val v1 = interp(t1, e)
      val e1 = e + (x -> v1)
      interp(t2, e1)
    case Fun(x, t) =>
      Closure(x, t, e)
    case App(t1, t2) =>
      val v1 = interp(t1, e)
      v1 match
        case Closure(id, t0, e0) =>
          val v2 = interp(t2, e)
          interp(t0, e0 + (id -> v2))
        case _ => throw new InterpError(s"unexpected function $v1 in $t")
    case Fix(x, t) =>
      interp(t, e + (x -> IceCube(x, t, e)))
    case FixFun(f, t) =>
      interp(t, e + (f -> IceCube(f, t, e)))
    case TList(ts) =>
      VList(ts.map(t => interp(t, e)))
    case Cons(t1, t2) =>
      val v = interp(t1, e)
      val VList(vs) = interp(t2, e) // interpreter t1 doit rendre une liste de valeurs
      VList(v :: vs)

