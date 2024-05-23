package tp05

sealed trait Term
case object EOF extends Term
case object TRUE extends Term
case object FALSE extends Term
case object ZERO extends Term
case class IfThenElse(condition: Term, thenBranch: Term, elseBranch: Term) extends Term
case class Succ(term: Term) extends Term
case class Pred(term: Term) extends Term
case class IsZero(term: Term) extends Term
