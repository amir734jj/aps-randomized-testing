import SimpleAst._
import java.util.concurrent.TimeUnit

object NestedCyclesDriver extends App {

  def time[R](str: String, func: () => R): (Long, R) = {
    val t0 = System.nanoTime()
    val re = func()
    val t1 = System.nanoTime()
    val seconds = TimeUnit.SECONDS.convert(t1 - t0, TimeUnit.NANOSECONDS)
    println(s"$str Elapsed time: " + seconds + " sec")

    (seconds, re)
  }

  def genTestbed(depth: Int): Map[String, () => _] = {
    val re = build(depth, 2)

    var m = Map[String, () => Any]()
    m = m + ("dynamic" -> (() => {
      val (_, tree) = re(0)
      val dynamicResult = new M_NESTED_CYCLES_DYNAMIC("Dynamic", tree.asInstanceOf[M_SIMPLE]);
      dynamicResult.finish()
      dynamicResult.v_msgs.asInstanceOf[Any]
    }))

    m = m + ("static" -> (() => {
      val (_, tree) = re(1)
      val staticResult = new M_NESTED_CYCLES_STATIC("Static", tree.asInstanceOf[M_SIMPLE]);
      staticResult.finish()
      staticResult.v_msgs.asInstanceOf[Any]
    }))

    m
  }

  for (i <- 1.0 to 8 by 1.0) {
    val depth = i.toInt
    println("AST Depth: " + depth)

    val result = genTestbed(depth)

    val (_, staticRe) = time("Static", result("static"))
    val (_, dynamicRe) = time("Dynamic", result("dynamic"))

    println("Result matching: " + dynamicRe.equals(staticRe) + "\n")
  }

}

