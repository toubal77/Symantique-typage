package tp09

import scala.collection.immutable.ListMap

sealed trait Term
case object EOF extends Term
case class Val(x : Var, t : Term) extends Term
case class Var(x : String) extends Term
case class Abs(x: Var, typ: Typ, t: Term) extends Term
case class App(t1: Term, t2: Term) extends Term
case object True extends Term
case object False extends Term
case class Cond(t1: Term, t2: Term, t3: Term) extends Term
case object Zero extends Term
case class Succ(t1: Term) extends Term
case class Pred(t1: Term) extends Term
case class IsZero(t1: Term) extends Term
case object U extends Term
case class LetIn(x: Var, t1: Term, t2: Term) extends Term
case class Fix(t1: Term) extends Term
case class Proj(t1: Term, lab: String) extends Term
case class Rec(fields: List[(String, Term)]) extends Term

sealed trait Typ
case object BoolType extends Typ
case object IntType extends Typ
case class Arrow(fromType: Typ, toType: Typ) extends Typ
case object Nat extends Typ
case class Lambda(paramType: Typ, returnType: Typ) extends Typ
case object UnitType extends Typ
case class RecordType(fields: ListMap[String, Typ]) extends Typ