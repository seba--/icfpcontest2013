package solver
package evaluation

trait SolverEvaluator {
  def benchmark[T](op: =>T) = {
    val start = System.currentTimeMillis()
    (op, System.currentTimeMillis()-start)
  }
  def evaluate(solver : Solver)
}