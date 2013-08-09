package lang;

import lang.FlatAbstract._
import lang.FlatAbstract.Node._

object Metadata {
  
  def size(e: Exp): Int = 1 + size_(e)._1
  
  def size_(e: Exp): (Int, Exp) = 
    if (e.head == Zero || e.head == One || e.head == MainVar || e.head == FoldNext || e.head == FoldAcc)
      (1, e.next)
    else if (e.head == IfZero) {
      val (cond, rest1) = size_(e.next)
      val (yes, rest2)  = size_(rest1)
      val (no, rest3)   = size_(rest2)
      (1+cond+yes+no, rest3)
    }
    else if (e.head == Fold) {
      val (over, rest1) = size_(e.next)
      val (init, rest2)  = size_(rest1)
      val (body, rest3)   = size_(rest2)
      (2+over+init+body, rest3)
    }
    else if (tryGetUnaryOp(e.head).isDefined) {
      val (v, rest) = size_(e.next)
      (1+v, rest)
    }
    else if (tryGetBinaryOp(e.head).isDefined) {
      val (v1, rest1) = size_(e.next)
      val (v2, rest2) = size_(rest1)
      (1+v1+v2, rest2)
    }
    else
      throw new IllegalArgumentException("unknown operator")


  def topOps(e: Exp): Set[Node] = {
    val os = ops(e)._1
    if (hasTopFold(e))
      os + TFold
    else
      os
  }
  
  def ops(e: Exp): (Set[Node], Exp) = 
    if (e.head == Zero || e.head == One || e.head == MainVar || e.head == FoldNext || e.head == FoldAcc)
      (Set[Node](), e.next)
    else if (e.head == IfZero || e.head == Fold) {
      val (o1, rest1):(Set[Node], Exp) = ops(e.next)
      val (o2, rest2):(Set[Node], Exp) = ops(rest1)
      val (o3, rest3):(Set[Node], Exp) = ops(rest2)
      (Set(e.head) ++ o1 ++ o2 ++ o3, rest3)
    }
    else if (tryGetUnaryOp(e.head).isDefined) {
      val (v, rest):(Set[Node], Exp) = ops(e.next)
      (Set(e.head) ++ v, rest)
    }
    else if (tryGetBinaryOp(e.head).isDefined) {
      val (v1, rest1):(Set[Node], Exp) = ops(e.next)
      val (v2, rest2):(Set[Node], Exp) = ops(rest1)
      (Set(e.head) ++ v1 ++ v2, rest2)
    }
    else
      throw new IllegalArgumentException("unknown operator")

  
  def hasTopFold(e: Exp) = 
    e.size >= 4 && e.head == Fold && e.next.head == MainVar && e.next.next.head == Zero
}