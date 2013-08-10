package solver
package evaluation

trait SolverEvaluator {
  def evaluate(solver: Solver)
}

case class Benchmarked[A](value: A, duration: Long) {
  def map[B](op: A => B) = Benchmarked(op(value), duration)
}

object Benchmarked {
  def apply[A](op: => A): Benchmarked[A] = {
    val start = System.currentTimeMillis()
    new Benchmarked(op, System.currentTimeMillis() - start)
  }
}