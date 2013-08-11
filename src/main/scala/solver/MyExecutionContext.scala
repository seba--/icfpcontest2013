package solver

import scala.actors.threadpool.Executors
import scala.concurrent.impl.ExecutionContextImpl
import scala.concurrent.ExecutionContext

object MyExecutionContext {
  private val pool = Executors.newCachedThreadPool()
  implicit val theContext = new ExecutionContext {
    def execute(runnable: Runnable) {
      pool.submit(runnable)
    }
    def reportFailure(t: Throwable) = {
      t.printStackTrace()
    }
  }
}