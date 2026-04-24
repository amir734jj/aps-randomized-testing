import SimpleAst._
import java.util.concurrent.TimeUnit

object NestedCyclesDriver extends App {

  private def time[R](str: String, func: () => R): (Double, R) = {
    val t0 = System.nanoTime()
    val re = func()
    val t1 = System.nanoTime()

    val seconds = (t1 - t0) / 1e9  // convert to seconds as Double
    println(f"$str Elapsed time: $seconds%.1f sec")

    (seconds, re)
  }

  def genTestbed(depth: Int): Map[String, () => _] = {
    val re = build(depth, 3)

    var m = Map[String, () => Any]()
    m = m + ("dynamic" -> (() => {
      val (_, tree) = re.head
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

    m = m + ("synth" -> (() => {
      val (_, tree) = re(2)
      val synthResult = new M_NESTED_CYCLES_SYNTH("Synth", tree.asInstanceOf[M_SIMPLE]);
      synthResult.finish()
      synthResult.v_msgs.asInstanceOf[Any]
    }))

    m
  }

  for (i <- 1.0 to 14 by 1.0) {
    val depth = i.toInt
    println("AST Depth: " + depth)

    val result = genTestbed(depth)

    val (_, staticRe) = time("Static", result("static"))
    val (_, dynamicRe) = time("Dynamic", result("dynamic"))
    val (_, synthRe) = time("Synth", result("synth"))

    if (staticRe.equals(synthRe)) {
      println("Static+Synth result matching")
    } else {
      throw new RuntimeException("Static+Synth result not matching")
    }

    if (dynamicRe.equals(staticRe)) {
      println("Dynamic+Static result matching")
    } else {
      throw new RuntimeException("Dynamic+Static result not matching")
    }

    if (dynamicRe.equals(synthRe)) {
      println("Dynamic+Synth result matching")
    } else {
      throw new RuntimeException("Dynamic+Synth result not matching")
    }

    println("\n")
  }

}

