package tp06

sealed abstract trait Term
case object EOF extends Term
case class Var(name : String) extends Term
case class Val(x : Var, t : Term) extends Term
case class Abs(v : Var, t : Term) extends Term
case class App(t1 : Term, t2 : Term) extends Term

