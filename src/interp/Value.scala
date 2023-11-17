package interp

import interp.Interp.Env
import ast.Term

// Scala 2
// sealed trait Value
// case class IntVal(i: Int) extends Value
// case class Closure(x: String, t: Term, e: Env) extends Value
enum Value :
  case IntVal(i: Int)
  case Closure(x: String, t: Term, e: Env)
  case VList(vs: List[Value])
  // which simply records the name of the predefined function
  case BuiltIn(name: String)
  override def toString: String = this match
    case IntVal(i) => i.toString
    case Closure(x, t, e) => s"$x -> $t with $e"
    case VList(vs) => vs.toString // TODO: améliorer l'impression des lists
    // simply records the name of the predefined function
    case BuiltIn(name) => s"$name"

case class IceCube(x: String, t: Term, e: Env)