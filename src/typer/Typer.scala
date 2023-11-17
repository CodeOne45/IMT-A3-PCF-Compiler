package typer

import ast.Op.*
import ast.Term
import Term.*
import unify.TVar

object Typer :
  type Env = Map[String, Type]
  val env0: Env =
    val a1 = TVar()
    val a2 = TVar()
    val a3 = TVar()
    Map[String, Type](
      "head" -> FUNCTION(LIST(a1), a1),
      "tail" -> FUNCTION(LIST(a2), LIST(a2)),
      "is_empty" -> FUNCTION(LIST(a3), INT),
  )

  def typer(t: Term, e: Env): Type = t match
    case Lit(_) => INT
    case BOp(op, t1, t2) =>
      val a = typer(t1, e)
      val b = typer(t2, e)
      if a === INT && b === INT then INT
      else throw new TypeError(s"term $t with environment $e")
/*      val v1 = interp(t1, e)
      val v2 = interp(t2, e)
      op match
        case PLUS => v1 + v2
        case MINUS => v1 - v2
        case TIMES => v1 * v2
        case DIVIDE => v1 / v2
*/
    case IfZ(test, t1, t2) =>
      val a = typer(test, e)
      val b = typer(t1, e)
      val c = typer(t2, e)
      if a === INT && b === c then b
      else throw new TypeError(s"term $t with environment $e")
/*      if interp(test, e) == IntVal(0) then interp(t1, e)
      else interp(t2, e)
*/
    case Var(x) =>
      e.get(x) match
        case Some(a: Type) => a
        case None => throw new TypeError(s"undefined variable $x in $e")
/*      e.get(x) match
        case Some(IceCube(x1, t1, e1)) => interp(Fix(x1, t1), e1)
        case Some(value: Value) => value
        case None => throw new InterpError(s"undefined variable $x in $e")
*/
    case Let(x, t1, t2) =>
      val A = typer(t1, e)
      val e1 = e + (x -> A)
      typer(t2, e1)
/*      val v1 = interp(t1, e)
      val e1 = e + (x -> v1)
      interp(t2, e1)
*/
    case Fun(x, t) =>
      val X = TVar()
      val e1 = e + (x -> X)
      val A = typer(t, e1)
      FUNCTION(X, A)


/*      Closure(x, t, e)
*/
    case App(t1, t2) =>
      val A = typer(t2, e)
      val B = typer(t1, e)
      val X = TVar()
      if B === FUNCTION(A, X) then X
      else throw new TypeError(s"term $t with environment $e")
/*      val v1 = interp(t1, e)
      v1 match
        case Closure(id, t0, e0) =>
          val v2 = interp(t2, e)
          interp(t0, e0 + (id -> v2))
        case _ => throw new InterpError(s"unexpected function $v1 in $t")
*/
    case Fix(x, t) =>
      val X = TVar()
      val e1 = e + (x -> X)
      val A = typer(t, e1)
      if X === A then A
      else throw new TypeError(s"term $t with environment $e")
/*      interp(t, e + (x -> IceCube(x, t, e)))
*/

    case FixFun(x,t) =>
      val X = TVar()
      val e1 = e + (x -> X)
      val A = typer(t, e1)
      if X === A then A
      else throw new TypeError(s"term $t with environment $e")

