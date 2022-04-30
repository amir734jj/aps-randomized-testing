import scala.util.Random

object SimpleAst {
  private final def genVariableName(depth: Int): String = {
    Random.nextString(depth)
  }

  // This is used for the width of the AST
  private val prob: Map[Int, Double] = Map(2 -> 0.5, 3 -> 0.3, 5 -> 0.2)

  private final def sample[A](dist: Map[A, Double]): A = {
    val p = Random.nextDouble()
    val it = dist.iterator
    var acc = 0.0
    while (it.hasNext) {
      val (item, itemProb) = it.next
      acc += itemProb
      if (acc >= p)
        return item // return so that we don't have to search through the whole distribution
    }
    sys.error(f"this should never happen") // needed so it will compile
  }

  private final def genDecls(depth: Int, ms: Seq[M_SIMPLE]): Seq[_] = {
    var prev = ms.map(m => m.v_no_decls().asInstanceOf[Any])

    if (depth > 0) {
      for (d <- 1 to sample(prob)) {
        val name = genVariableName(depth)

        for ((m, i) <- ms.zipWithIndex) {
          val ty = if ((d + depth) % 2 == 0) m.v_string_type() else m.v_integer_type()

          prev = prev.updated(i, m.v_xcons_decls(prev(i).asInstanceOf[m.T_Decls], m.v_decl(name, ty)))
        }
      }
    }

    prev
  }

  private final def genStmts(depth: Int, ms: Seq[M_SIMPLE]): Seq[_] = {
    var prev = ms.map(m => m.v_no_stmts().asInstanceOf[Any])

    if (depth > 0) {
      for (d <- 1 to sample(prob)) {
        val name = genVariableName(depth)

        for ((m, i) <- ms.zipWithIndex) {
          val v = if ((d - depth) % 2 == 0) m.v_strconstant(i.toString) else m.v_intconstant(i)

          prev = prev.updated(i, m.v_xcons_stmts(prev(i).asInstanceOf[m.T_Stmts], m.v_assign_stmt(m.v_variable(name), v)))
        }
      }

      for (_ <- 1 to sample(prob)) {
        val bs = genBlock(depth, ms)

        for ((m, i) <- ms.zipWithIndex) {
          prev = prev.updated(i, m.v_xcons_stmts(prev(i).asInstanceOf[m.T_Stmts], m.v_block_stmt(bs(i).asInstanceOf[m.T_Block])))
        }
      }
    }

    prev
  }

  private final def genBlock(depth: Int, ms: Seq[M_SIMPLE]): Seq[_] = {
    assert(depth > 0, "Depth parameter passed into genBlock should be greater than one.")

    val ds = genDecls(depth - 1, ms)
    val ss = genStmts(depth - 1, ms)

    for ((m, i) <- ms.zipWithIndex) yield m.v_block(ds(i).asInstanceOf[m.T_Decls], ss(i).asInstanceOf[m.T_Stmts])
  }

  private final def genProgram(depth: Int, ms: Seq[M_SIMPLE]): Seq[_] = {
    val bs = genBlock(depth, ms)

    for ((m, i) <- ms.zipWithIndex) yield m.v_program(bs(i).asInstanceOf[m.T_Block])
  }

  final def build(depth: Int, count: Int): Seq[(_, _)] = {
    val tms = (1 to count)
      .map(i => new M_SIMPLE(f"simple$i"))
      .map(m => (m, m.t_Result))

    genProgram(depth, tms.map(t => t._2))

    for ((m, _) <- tms) {
      m.finish()
    }

    tms
  }
}
