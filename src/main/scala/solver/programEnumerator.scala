package solver

import lang.Abstract._

object ProgramEnumerator {
	
  def next(e : Exp) : Exp = {
    if (e == null) return Zero()
    
    e match {
      case Zero() => One()
      case One() => Var("x")
    }
  }
}