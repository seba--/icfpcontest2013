package solver.old

import lang.Abstract._
import lang.Abstract.Operator._
import lang.Semantics._
import scala.collection.mutable

object Tester extends App {
  println(new Solver(1, List()).solve(Map(1l -> 0l, 2l -> 0l, 3l -> 0l)) + " == " + Zero())
  println(new Solver(1, List()).solve(Map(1l -> 1l, 2l -> 1l, 3l -> 1l)) + " == " + One())
}

class Solver(size : Int, operators: List[Operator]) {

  private var restSize = size
  private val solution = Box()
  private var queue = mutable.DoubleLinkedList[Box](solution)
  private var solutionSize = 0
  
  def minRestSizeRequired = queue.size
  
  def solve(knowledge : Map[Long, Long]): Exp = {
    while ((queue.elem != null && queue.elem.isEmpty) || restSize > 0 || !test(knowledge)) {
      if (queue.elem == null) {
        queue = queue.prev
      }
      else {
	    val box = queue.elem
	    queue = queue.next
	    
	    box.e = Zero()
	    restSize -= 1
      }
    }
    
  	return solution
  }
  
  def test(knowledge : Map[Long, Long]) : Boolean = {
    val prg = Prg("test", solution)
    knowledge foreach ((tuple) => (if (eval(prg)(tuple._1) != tuple._2) return false))
    true
  }
}