package typer

import unify.{Atom, HTerm, Structure, TVar}

type Type = HTerm
object INT extends Atom("INT") :
  override def toString = "INT"
class FUNCTION(a: Type, b: Type) extends Structure("function", List(a, b)) :
  override def toString = s"($a -> $b)"
  val from = a
  val to = b
class LIST(a: Type) extends Structure("list", List(a)):
  val currentType = a
  override def toString: String = s"[$a]"


class BUILTIN(func : (Type) => Type) extends Structure("function", (() =>
  val anType = TVar()
  List(anType, func(anType)))()):
  val typeFunc = func
  override def toString = s"(${this.types.head} -> ${this.types(1)})"

object BUILTIN {
  def unapply(a : BUILTIN) : Option[(Type) => Type] = Some(a.typeFunc)
}

object FUNCTION {
  def unapply(a : FUNCTION) : Option[(Type, Type)] = Some((a.from, a.to))
}

object LIST {
  def unapply(a : LIST) : Option[Type] = Some(a.currentType)
}
object Test extends App :
  val a = INT
  val b = INT
  println(s"Is a === b true? ${a === b}")
  val v = TVar()
  println(s"variable v created, v = $v")
  println(s"Is v === a true? ${v === a}")
  println(s"v = $v")
  val v1 = TVar()
  println(s"variable v1 created, v1 = $v1")
  val v2 = TVar()
  println(s"variable v2 created, v2 = $v2")
  println(s"Is v1 === v2 true? ${v1 === v2}")
  println(s"v1 = $v1")
  println(s"v2 = $v2")
  println(s"Is v1 === v2 true? ${v1 === v2}")
  println(s"Is v1 === a true? ${v1 === a}")
  println(s"v1 = $v1")
  println(s"v2 = $v2")
  val v3 = TVar()
  val v4 = TVar()
  val a1 = FUNCTION(v3, TVar())
  val a2 = FUNCTION(TVar(), v4)
  println(s"a1 = $a1")
  println(s"a2 = $a2")
  v3 === v4
  println("v3 === v4")
  println(s"a1 = $a1")
  println(s"a2 = $a2")
  a1 === a2
  println("a1 === a2")
  println(s"a1 = $a1")
  println(s"a2 = $a2")
  v3 === INT
  println(s"a1 = $a1")
  println(s"a2 = $a2")

