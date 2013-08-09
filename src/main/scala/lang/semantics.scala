package lang

import lang.FlatAbstract._
import lang.FlatAbstract.Node._
import lang.Concrete._

object Semantics {

  case class UnexpectedOperator(expected: String, op: Node) extends Exception
  case class EmptyBox() extends Exception

  type Value = Long

  def toString(v: Value): String = "0x%016X".format(v)
  def fromString(s: String): Value = {
    val rest = s.substring(3)
    s.charAt(2) match {
      case '8' => Long.MinValue + java.lang.Long.parseLong(rest, 16)
      case '9' => Long.MinValue + java.lang.Long.parseLong("1" + rest, 16)
      case 'A' => Long.MinValue + java.lang.Long.parseLong("2" + rest, 16)
      case 'B' => Long.MinValue + java.lang.Long.parseLong("3" + rest, 16)
      case 'C' => Long.MinValue + java.lang.Long.parseLong("4" + rest, 16)
      case 'D' => Long.MinValue + java.lang.Long.parseLong("5" + rest, 16)
      case 'E' => Long.MinValue + java.lang.Long.parseLong("6" + rest, 16)
      case 'F' => Long.MinValue + java.lang.Long.parseLong("7" + rest, 16)
      case _ => java.lang.Long.parseLong(s.substring(2), 16)
    }
  }

  def eval(p: Exp)(v: Value): Value = {
    val (result, rest) = new Eval(v).eval(p)
    if (!rest.isEmpty)
      throw UnexpectedOperator("end of input", rest.head)
    result
  }

  class Eval(in: Value) {
    private var foldNext: Option[Value] = None
    private var foldAcc: Option[Value] = None

    def eval(e: Exp): (Value, Exp) = 
      if (e.head == Zero)
        (0L, e.next)
      else if (e.head == One) 
        (1L, e.next)
      else if (e.head == MainVar) 
        (in, e.next)
      else if (e.head == FoldNext && foldNext.isDefined)
        (foldNext.get, e.next)
      else if (e.head == FoldAcc && foldAcc.isDefined)
        (foldAcc.get, e.next)
      else if (e.head == FoldNext || e == FoldAcc)
        throw UnexpectedOperator("Variable not bound.", e.head)
      else if (e.head == IfZero) {
        val (cval, rest1) = eval(e.next)
        if (cval == 0) {
          val (yesval, rest2) = eval(rest1)
          (yesval, makeStructural(rest2)._2)
        }
        else
          eval(makeStructural(rest1)._2)
      }

      else if (e.head == Fold) {
        val (v, rest1) = eval(e.next)
        val (initAcc, body) = eval(rest1)

        val b0 = (v & 0x00000000000000FFL)
        val b1 = (v & 0x000000000000FF00L) >>> 8
        val b2 = (v & 0x0000000000FF0000L) >>> 16
        val b3 = (v & 0x00000000FF000000L) >>> 24
        val b4 = (v & 0x000000FF00000000L) >>> 32
        val b5 = (v & 0x0000FF0000000000L) >>> 40
        val b6 = (v & 0x00FF000000000000L) >>> 48
        val b7 = (v & 0xFF00000000000000L) >>> 56

        foldAcc = Some(initAcc)

        foldNext = Some(b0)
        foldAcc = Some(eval(body)._1)
        foldNext = Some(b1)
        foldAcc = Some(eval(body)._1)
        foldNext = Some(b2)
        foldAcc = Some(eval(body)._1)
        foldNext = Some(b3)
        foldAcc = Some(eval(body)._1)
        foldNext = Some(b4)
        foldAcc = Some(eval(body)._1)
        foldNext = Some(b5)
        foldAcc = Some(eval(body)._1)
        foldNext = Some(b6)
        foldAcc = Some(eval(body)._1)
        foldNext = Some(b7)
        val result = eval(body)

        foldNext = None
        foldAcc = None

        result
      }

      else if (tryGetUnaryOp(e.head).isDefined) {
        val (v, rest) = eval(e.next)
        (e.head match {
          case Not => ~v
          case Shl1 => v << 1
          case Shr1 => v >>> 1
          case Shr4 => v >>> 4
          case Shr16 => v >>> 16
          case _ => throw UnexpectedOperator("unary", e.head)
        }, rest)
      }
        

      else if (tryGetBinaryOp(e.head).isDefined) {
        val (v1, rest1) = eval(e.next)
        val (v2, rest2) = eval(rest1)
        (e.head match {
          case And => v1 & v2
          case Or => v1 | v2
          case Xor => v1 ^ v2
          case Plus => v1 + v2
          case _ => throw UnexpectedOperator("binary", e.head)
        }, rest2)
      }
    
      else throw UnexpectedOperator("unknown operator", e.head)
  }

}