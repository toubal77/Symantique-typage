package tp05

import Util._

class Evaluator {
  private def eval(t: Term): Term = t match {
    case IfThenElse(condition, thenBranch, elseBranch) =>
      eval(condition) match {
        case TRUE => eval(thenBranch)
        case FALSE => eval(elseBranch)
        case _ => throw new IllegalArgumentException("Condition must be a boolean")
      }
    case Succ(t1) => t1 match {
      case ZERO => Succ(ZERO)
      case Pred(v) if isNumericValue(v) => v
      case _ => Succ(eval(t1))
    }
    case Pred(t1) => t1 match {
      case ZERO => ZERO
      case Succ(v) if isNumericValue(v) => v
      case _ => Pred(eval(t1))
    }
    case IsZero(t1) => t1 match {
      case ZERO => TRUE
      case Succ(v) if isNumericValue(v) => FALSE
      case _ => IsZero(eval(t1))
    }
    case _ => throw new IllegalArgumentException("Invalid term")
  }

  def evaluate(t: Term): Term = {
    if (isVal(t)) t else evaluate(eval(t))
  }

  private def isNumericValue(t: Term): Boolean = t match {
    case ZERO => true
    case Succ(v) => isNumericValue(v)
    case _ => false
  }
}
