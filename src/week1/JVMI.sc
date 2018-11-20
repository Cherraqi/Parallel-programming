
object JVMI {
  println("Hello World")
  val a = new Thread()
  a.start()
  /*****   What is a Thread *****/
  class HelloThread extends Thread {
    override def run() {
      println("Hello World!") //what the thread does
    }
  }
  class HelloThread2 extends Thread {
    override def run() {
      println("Hello")
      println("World!")
    }
  }
  val t = new HelloThread()
  val s1 = new HelloThread2()
  val s2 = new HelloThread2()
  t.start()
  s1.start()
  s2.start()
  t.join()
  s1.join()
  s2.join()
  /****** Asynchrnously generate unique IDs *******/
  /***threads are inherently asynchronous***/

  var uidCountAsync = 0L


  def getUniqueIdAsync(): Long = {
    uidCountAsync = uidCountAsync+1
    uidCountAsync
  }


  def startThreadAsync() = {
    val t = new Thread {
      override def run() {
        val uids = for (i <- 0 until 10) yield getUniqueIdAsync()
        println(uids)
      }
    }
    t.start()
    t
  }

  val asyncId1 = startThreadAsync()
  val asyncId2 = startThreadAsync()

  /***both threads could output 1 if asyncId2 executes before asyncId1 finishes execution***/
  /***because they are asynchronous and both mutates var uidCountAsync**/
  asyncId1.join()
  asyncId2.join()

  /****** Synchrnously generate unique IDs *******/
  var uidCountSync = 0L
  //x is called a monitor
  val x = new AnyRef {}

  def getUniqueIdAtomic(): Long = x.synchronized {
    uidCountSync = uidCountSync+1
    uidCountSync
  }


  def startThreadSync() = {
    val t = new Thread {
      override def run() {
        val uids = for (i <- 0 until 10) yield getUniqueIdAtomic()
        println(uids)
      }
    }
    t.start()
    t
  }


  val syncId1 = startThreadSync()
  val syncId2 = startThreadSync()


  syncId1.join()
  syncId2.join()

}