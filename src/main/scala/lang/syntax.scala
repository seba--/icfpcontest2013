package lang

import scala.collection.mutable.DoubleLinkedList

object Abstract {
  
  type Id = String

  object Operator extends Enumeration {
    val Not, Shl1, Shr1, Shr4, Shr16 = Value
    val And, Or, Xor, Plus = Value
    val If0, Fold, TFold = Value
  }
  type Operator = Operator.Value

  def printProgram(e: Exp) = "(lambda (" + "main_var" + ") " + e + ")" 
  
  abstract class Exp
  case class Zero() extends Exp { override def toString = "0" }
  case class One() extends Exp { override def toString = "1" }
  case class MainVar() extends Exp { override def toString = "main_var" }
  case class FoldNext() extends Exp { override def toString = "fold_next" }
  case class FoldAcc() extends Exp { override def toString = "fold_var" }
  case class IfZero(var cond: Exp, var yes: Exp, var no: Exp) extends Exp { override def toString = "(if0 " + cond + " " + yes + " " + no + ")" }
  case class Fold(var over: Exp, var init: Exp, var body: Exp) extends Exp { override def toString = "(fold " + over+ " " + init + " " + "(lambda (" + "fold_next" + " " + "fold_acc" + ") " + body + ")" + ")" }
  case class UApp(var op: Operator, var e: Exp) extends Exp { override def toString = "(" + op.toString.toLowerCase + " " + e + ")" }
  case class BApp(var op: Operator, var e1: Exp, var e2: Exp) extends Exp { override def toString = "(" + op.toString.toLowerCase + " " + e1 + " " + e2 + ")" }
  case class Box() extends Exp {
    var e : Exp = null
    def isEmpty : Boolean = e == null    
    override def toString = {
      if (isEmpty) "?" else e.toString()
    }
  }
}


//object FlatAbstract {
//  
//  object Node extends Enumeration {
//    val Zero, One, MainVar = Value
//    val IfZero, Fold, FoldNext, FoldAcc = Value
//    val Not, Shl1, Shr1, Shr4, Shr16 = Value
//    val And, Or, Xor, Plus = Value
//    val TFold = Value
//  }
//  type Node = Node.Value
//  import Node._
//  
//  def getNode(s: String) = s match {
//    case "not" => Not
//    case "shl1" => Shl1
//    case "shr1" => Shr1
//    case "shr4" => Shr4
//    case "shr16" => Shr16
//    case "and" => And
//    case "or" => Or
//    case "xor" => Xor
//    case "plus" => Plus
//    case "if0" => IfZero
//    case "fold" => Fold
//    case "tfold" => TFold
//  }
//
//  
//  type Exp = DoubleLinkedList[Node]
//  
//  
//  def makeFlat(p: Abstract.Prg): Exp = makeFlat(p.e, p.x)
//  
//  def makeFlat(
//      e: Abstract.Exp, 
//      mainVar: Abstract.Id,
//      foldNextVar: Abstract.Id = null, 
//      foldAccVar: Abstract.Id = null): Exp = e match {
//    case Abstract.Zero() => DoubleLinkedList(Zero)
//    case Abstract.One() => DoubleLinkedList(One)
//    case Abstract.Var(v) if foldNextVar != null && v == foldNextVar => DoubleLinkedList(FoldNext)
//    case Abstract.Var(v) if foldAccVar != null && v == foldAccVar => DoubleLinkedList(FoldAcc)
//    case Abstract.Var(v) if v == mainVar => DoubleLinkedList(MainVar)
//    case Abstract.Var(v) => sys.error("Cannot convert unknown variable " + v)
//    case Abstract.IfZero(cond, yes, no) => (DoubleLinkedList(IfZero) ++ makeFlat(cond, mainVar, foldNextVar, foldAccVar)
//                                                                     ++ makeFlat(yes, mainVar, foldNextVar, foldAccVar)
//                                                                     ++ makeFlat(no, mainVar, foldNextVar, foldAccVar))
//    case Abstract.Fold(over, init, Abstract.FoldFun(next, acc, body)) =>
//      (DoubleLinkedList(Fold) ++ makeFlat(over, mainVar, foldNextVar, foldAccVar)
//                              ++ makeFlat(init, mainVar, foldNextVar, foldAccVar)
//                              ++ makeFlat(body, mainVar, next, acc))
//    case Abstract.UApp(op, e) =>
//      DoubleLinkedList(makeUnaryOp(op)) ++ makeFlat(e, mainVar, foldNextVar, foldAccVar)
//    case Abstract.BApp(op, e1, e2) =>
//      DoubleLinkedList(makeBinaryOp(op)) ++ makeFlat(e1, mainVar, foldNextVar, foldAccVar) ++ makeFlat(e2, mainVar, foldNextVar, foldAccVar)
//  }
//  
//  def makeUnaryOp(op: Abstract.Operator) = 
//    op match {
//      case Abstract.Operator.Not => Not
//      case Abstract.Operator.Shl1 => Shl1
//      case Abstract.Operator.Shr1 => Shr1
//      case Abstract.Operator.Shr4 => Shr4
//      case Abstract.Operator.Shr16=> Shr16
//    }
//
//  def makeBinaryOp(op: Abstract.Operator) = 
//    op match {
//      case Abstract.Operator.And => And
//      case Abstract.Operator.Or => Or
//      case Abstract.Operator.Xor => Xor
//      case Abstract.Operator.Plus => Plus
//    }
//  
//  def makeStructuralPrg(e: Exp): Abstract.Prg = {
//    val (exp, rest) = makeStructural(e)
//    if (!rest.isEmpty)
//      throw new IllegalStateException("Expected empty rest list")
//    Abstract.Prg("main_var", exp)
//  }
//  
//  def makeStructural(e: Exp): (Abstract.Exp, Exp) = 
//    if (e.head == Zero)
//      (Abstract.Zero(), e.next)
//    else if (e.head == One)
//      (Abstract.One(), e.next)
//    else if (e.head == MainVar)
//      (Abstract.Var("main_var"), e.next)
//    else if (e.head == FoldNext)
//      (Abstract.Var("fold_next"), e.next)
//    else if (e.head == FoldAcc)
//      (Abstract.Var("fold_acc"), e.next)
//    else if (e.head == IfZero) {
//      val (cond, rest1) = makeStructural(e.next)
//      val (yes, rest2)  = makeStructural(rest1)
//      val (no, rest3)   = makeStructural(rest2)
//      (Abstract.IfZero(cond, yes, no), rest3)
//    }
//    else if (e.head == Fold) {
//      val (over, rest1) = makeStructural(e.next)
//      val (init, rest2)  = makeStructural(rest1)
//      val (body, rest3)   = makeStructural(rest2)
//      (Abstract.Fold(over, init, Abstract.FoldFun("fold_next", "fold_acc", body)), rest3)
//    }
//    else {
//      val (e1, rest1) = makeStructural(e.next)
//      tryGetUnaryOp(e.head) match {
//        case Some(op) => (Abstract.UApp(op, e1), rest1)
//        case None => tryGetBinaryOp(e.head) match {
//          case Some(op) => {
//            val (e2, rest2) = makeStructural(rest1)
//            (Abstract.BApp(op, e1, e2), rest2)
//          }
//          case None => sys.error("illegal operator " + e.head)
//        }
//      }
//    } 
//  
//  def tryGetUnaryOp(n: Node) = n match {
//    case Not => Some(Abstract.Operator.Not)
//    case Shl1 => Some(Abstract.Operator.Shl1)
//    case Shr1 => Some(Abstract.Operator.Shr1)
//    case Shr4 => Some(Abstract.Operator.Shr4)
//    case Shr16 => Some(Abstract.Operator.Shr16)
//    case _ => None
//  }
//
//  def tryGetBinaryOp(n: Node) = n match {
//    case And => Some(Abstract.Operator.And)
//    case Or => Some(Abstract.Operator.Or)
//    case Xor => Some(Abstract.Operator.Xor)
//    case Plus => Some(Abstract.Operator.Plus)
//    case _ => None
//  }
//}



object Concrete {
  
  import Abstract._
  
  case class ParseException(msg: String, rest: String) extends Exception
  
  type Result[A] = (A, String)
  
  def parse(s: String): Exp = {
    val (p, rest) = parsePrg(s)
    if (!rest.isEmpty)
      throw ParseException("Expected end of file", rest)
    p
  }
  
  def parsePrg: String => Result[Exp] =
    inParens(s => {
      val (_, s1) = word("lambda")(layout(s))
      val (x, s2) = inParens(parseId)(layout(s1))
      val (e, s3) = parseExp(layout(s2), x, null, null)
      (e, s3)
    })
      
  
  def parseId(s: String): Result[String] = {
    var length: Int = 0
    while (length < s.size) {
      val c = s(length)
      if ((c >= 'a' && c <= 'z') || c == '_' || (c >= '0' && c <= '9'))
        length += 1
      else
        return if (length > 0) 
                 (s.substring(0, length), s.substring(length))
               else
                 throw ParseException("Expected identifier", s)
    }
    return if (length > 0) 
             (s.substring(0, length), s.substring(length))
           else
             throw ParseException("Expected identifier", s)
  }
  
  def parseExp(s: String, mainVar: String, foldNext: String, foldVar: String): Result[Exp] =
    if (s.startsWith("0"))
      (Zero(), s.substring(1))
    else if (s.startsWith("1"))
      (One(), s.substring(1))
    else if (s.size > 0 && (s(0) >= 'a' && s(0) <= 'z')) {
      val (x, s2) = parseId(s)
      if (x == foldNext)
        (FoldNext(), s2)
      else if (x == foldVar)
        (FoldAcc(), s2)
      else if (x == mainVar)
        (MainVar(), s2)
      else throw ParseException("Unbound variable " + x, s2)
    }
    else
      inParens(parseExp1(mainVar, foldNext, foldVar))(s)
  
  def parseExp1(mainVar: String, foldNext: String, foldVar: String)(s: String): Result[Exp] =
    if (s.startsWith("if0")) {
      val (_, s1) = word("if0")(layout(s))
      val (e1, s2) = parseExp(layout(s1), mainVar, foldNext, foldVar)
      val (e2, s3) = parseExp(layout(s2), mainVar, foldNext, foldVar)
      val (e3, s4) = parseExp(layout(s3), mainVar, foldNext, foldVar)
      (IfZero(e1, e2, e3), s4)
    }
    else if (s.startsWith("fold")) {
      val (_, s1) = word("fold")(layout(s))
      val (e1, s2) = parseExp(layout(s1), mainVar, foldNext, foldVar)
      val (e2, s3) = parseExp(layout(s2), mainVar, foldNext, foldVar)
      val (f, s4) = inParens(parseFoldFun(mainVar))(layout(s3))
      (Fold(e1, e2, f), s4)
    }
    else {
      tryParseUnary(layout(s)) match {
        case Some((op, s2)) => {
          val (e, s3) = parseExp(layout(s2), mainVar, foldNext, foldVar)
          (UApp(op, e), s3)
        }
        case None =>
          tryParseBinary(layout(s)) match {
            case Some((op, s2)) => {
              val (e1, s3) = parseExp(layout(s2), mainVar, foldNext, foldVar)
              val (e2, s4) = parseExp(layout(s3), mainVar, foldNext, foldVar)
              (BApp(op, e1, e2), s4)
            }
            case None => throw ParseException("Expected expression", s)
          }
      }
    }

  def tryParseUnary(s: String): Option[Result[Operator]] = {
    val (op, s2) = parseId(layout(s))
    val operator = 
    if (op == "not")
      Operator.Not
    else if (op == "shl1")
      Operator.Shl1 
    else if(op == "shr1")
      Operator.Shr1
    else if (op == "shr4")
      Operator.Shr4
    else if (op == "shr16")
      Operator.Shr16
    else 
      null
    if (operator != null)
      Some((operator, s2))
    else
      None
  }

  def tryParseBinary(s: String): Option[Result[Operator]] = {
    val (op, s2) = parseId(layout(s))
    val operator = 
    if (op == "and")
      Operator.And
    else if (op == "or")
      Operator.Or 
    else if(op == "xor")
      Operator.Xor
    else if (op == "plus")
      Operator.Plus
    else 
      null
    if (operator != null)
      Some((operator, s2))
    else
      None
  }
  
  def tryParseOperator(s: String): Option[Operator] =
    tryParseUnary(s) match {
      case Some((op,_)) => Some(op)
      case None => tryParseBinary(s) match {
        case Some((op,_)) => Some(op)
        case None =>
          if (s == "if0")
            Some(Operator.If0)
          else if (s == "fold")
            Some(Operator.Fold)
          else if (s == "tfold")
            Some(Operator.TFold)
          else
            None
      }
    }

  def parseFoldFun(mainVar: String)(s: String): Result[Exp] = {
    val (_, s1) = word("lambda")(layout(s))
    val ((x,y), s2) = inParens({s =>
        val (x, s21) = parseId(layout(s))
        val (y, s22) = parseId(layout(s21))
        ((x, y), s22)
      })(layout(s1))
    val (e, s3) = parseExp(layout(s2), mainVar, x, y)
    (e, s3)
  }
    
  def layout(s: String): String = {
    var length: Int = 0
    while (length < s.size) {
      val c = s(length)
      if (c == ' ' || c == '\n' || c == '\r' || c == '\t')
        length += 1
      else
        return s.substring(length)
    }
    return s.substring(length)
  } 
  
  def word(w: String)(s: String): Result[Unit] = 
    if (s.startsWith(w)) {
      val rest = s.substring(w.size)
      if (rest.size > 0 && ((rest(0) >= 'a' && rest(0) <= 'z') || rest(0) == '_' || (rest(0) >= '0' && rest(0) <= '9')))
        throw ParseException("Expected " + w, s)
      else
        ((), rest)
    }
    else
      throw ParseException("Expected " + w, s)
  
  def inParens[A](parser: String => Result[A])(s: String): Result[A] = {
    val size = s.size
    if (size >= 2 && s(0) == '(') {
      val (a, rest) = parser(layout(s.substring(1)))
      val r = layout(rest)
      if (r.size > 0 && r(0) == ')')
        return (a, r.substring(1))
      else
        throw ParseException("Expected closing parenthesis", r)  
    }
    throw ParseException("Expected opening parenthesis", s)
  }
}

