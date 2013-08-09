package lang;

import lang.Abstract._

object Metadata {
  
  def size(p: Prg): Int = 1 + size(p.e)
  
  def size(e: Exp): Int = e match {
    case b@Box() => if (b.isEmpty) 0 else size(b.e)
    case Zero() => 1
    case One() => 1
    case Var(_) => 1
    case IfZero(e0, e1, e2) => 1 + size(e0) + size(e1) + size(e2)
    case Fold(e0, e1, FoldFun(_, _, e2)) => 2 + size(e0) + size(e1) + size(e2)
    case UApp(_, e) => 1 + size(e)
    case BApp(_, e1, e2) => 1 + size(e1) + size(e2)
  }
  
  def ops(p: Prg): Set[Operator] = {
    val os = ops(p.e)
    if (hasTopFold(p))
      os + Operator.TFold
    else
      os
  }
  
  def ops(e: Exp): Set[Operator] = e match {
    case b@Box() => if (b.isEmpty) Set() else ops(b.e)
    case Zero() => Set()
    case One() => Set()
    case Var(_) => Set()
    case IfZero(e0, e1, e2) => Set(Operator.If0) ++ ops(e0) ++ ops(e1) ++ ops(e2)
    case Fold(e0, e1, FoldFun(_, _, e2)) => Set(Operator.Fold) ++ ops(e0) ++ ops(e1) ++ ops(e2)
    case UApp(op, e) => Set(op) ++ ops(e)
    case BApp(op, e1, e2) => Set(op) ++ ops(e1) ++ ops(e2)
  }
  
  def hasTopFold(p: Prg) = p match {
    case Prg(x1, Fold(x2, Zero(), FoldFun(x3, y, e))) if x1 == x2 && x2 == x3 => true
    case _ => false
  }
}