import org.scalameter._
import java.util.concurrent._
import scala.util.DynamicVariable

package object common {

  val forkJoinPool = new ForkJoinPool

  abstract class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T]
    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      val right = task {
        taskB
      }
      val left = taskA
      (left, right.join())
    }
  }

  class DefaultTaskScheduler extends TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute = body
      }
      Thread.currentThread match {
        case wt: ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }
      t
    }
  }

  val scheduler =
    new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

  /** from the common package
    *  task takes one by-name parameter, body */
  def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }

  /** from the common package */
  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    scheduler.value.parallel(taskA, taskB)
  }

  def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
    val ta = task { taskA }
    val tb = task { taskB }
    val tc = task { taskC }
    val td = taskD
    (ta.join(), tb.join(), tc.join(), td)
  }
}


object testTask {
  //Testing benchmarking and the computation time of val, lazy val, and def

  def f[T] = (v: T) => v+"!"
  def f2[T](v: => T) = v+"!"

  //These guys have to be def beause if they were val, then they get evaluated the first time
  //they are declared. We want to measure the evaluation inside the scalameter
  def e1 = "hello".toList.map(f(_))
  val e11 = "hello".toList.map(f(_))
  lazy val e12 = "hello".toList.map(f(_))

  def e13 = "hello".toList.map(f2(_))
  val e14 = "hello".toList.map(f2(_))

  def e2 = "world".toList.map(a => f(a))
  def e3 = f("hello")
  def e4 = f("world")

  val (v1, v2) = parallel(e1, e2)


  //lazy val - first time slow, then subsequent evaluations are fast because of memoization
  //computation first time it's called below.
  val time12 = withWarmer(new Warmer.Default) measure { e12 }

  val time12a = withWarmer(new Warmer.Default) measure { e12 }

  val time12b = withWarmer(new Warmer.Default) measure { e12 }


  //def - slow everytime because recomputes everytime you call it
  val time1 = withWarmer(new Warmer.Default) measure { e1 }

  val time1a = withWarmer(new Warmer.Default) measure { e1 }

  val time1b = withWarmer(new Warmer.Default) measure { e1 }


  //val - everytime fast because computation is performed when the val was first declared above
  val time11 = withWarmer(new Warmer.Default) measure { e11 }

  val time11a = withWarmer(new Warmer.Default) measure { e11 }

  val time11b = withWarmer(new Warmer.Default) measure { e11 }


  //def and by-name
  val time13 = withWarmer(new Warmer.Default) measure { e13 }

  val time13a = withWarmer(new Warmer.Default) measure { e13 }

  val time13b = withWarmer(new Warmer.Default) measure { e13 }

  //val and by-name
  val time14 = withWarmer(new Warmer.Default) measure { e14 }

  val time14a = withWarmer(new Warmer.Default) measure { e14 }

  val time14b = withWarmer(new Warmer.Default) measure { e14 }


  val time2 = withWarmer(new Warmer.Default) measure { e2 }

  val time3 = withWarmer(new Warmer.Default) measure { e3 }

  val time4 = withWarmer(new Warmer.Default) measure { e4 }

  println(s"time1 = $time1 ms, time2 = $time2 ms")

  println(s"time3 = $time3 ms, time4 = $time4 ms")



  val ((part1, part2), (part3, part4)) = parallel(parallel(e1, e2),parallel(e3, e4))

   withWarmer(new Warmer.Default) measure { e4 }
  val (part5, part6, part7, part8) = parallel(e1, e2, e3, e4)




}

