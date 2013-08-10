package lang;

import lang.Abstract._

object Metadata {
  
  def size(e: Exp): Int = 1 + size_(e)
  
  def size_(e: Exp): Int = e match {
    case b@Box() => if (b.isEmpty) 0 else size_(b.e)
    case Zero() => 1 
    case One() => 1
    case MainVar() => 1
    case FoldNext() => 1
    case FoldAcc() => 1
    case IfZero(cond, yes, no) => 1 + size_(cond) + size_(yes) + size_(no)
    case Fold(over, init, body) => 2 + size(over) + size_(init) + size_(body)
    case UApp(op, e) => 1 + size_(e)
    case BApp(op, e1, e2) => 1 + size_(e1) + size_(e2)
  }

  def topOps(e: Exp): Set[Operator] = {
    val os = ops(e)
    if (hasTopFold(e))
      os + Operator.TFold
    else
      os
  }
  
  def ops(e: Exp): Set[Operator] = e match {
    case b@Box() => if (b.isEmpty) Set() else ops(b.e)
    case Zero() => Set[Operator]() 
    case One() => Set()
    case MainVar() => Set()
    case FoldNext() => Set()
    case FoldAcc() => Set()
    case IfZero(cond, yes, no) => Set(Operator.If0) ++ ops(cond) ++ ops(yes) ++ ops(no)
    case Fold(over, init, body) => Set(Operator.Fold) ++ ops(over) ++ ops(init) ++ ops(body)
    case UApp(op, e) => Set(op) ++ ops(e)
    case BApp(op, e1, e2) => Set(op) ++ ops(e1) ++ ops(e2)
  }

  
  def hasTopFold(e: Exp) = e match {
    case Fold(MainVar(), Zero(), _) => true
    case _ => false
  } 
}