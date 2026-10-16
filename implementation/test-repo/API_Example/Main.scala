import unaa_lola.DSL.DSL
import unaa_lola.DSL.ExpressionBuilder._
import unaa_lola.Value._
import unaa_lola.Implementations
import unaa_lola.Config
import unaa_lola.Evaluator

object Main extends App {
  val x = stream("x", Real)
  val y = stream("y", Real)
  val ok = stream("ok", Bool)
  val staysOk = stream("stays_ok", Bool)
  val nextX = stream("nextX", Real)

  val spec = DSL.spec
    .input(x)
    .define(y, y.prev(0) + x - x.at(-3,0))
    .define(ok, y <= 30)
    .define(staysOk, ok && staysOk.next(true))
    .output(y, ok, staysOk)
    .assumption((x.next(0) - 5 <= x) && (x <= x.next(0) + 5) && x <= 20)
    .define(nextX, x.next(0))
    .linked(nextX, staysOk)
    .output(nextX)
    .build

  println("Spec:")
  println(spec)
  println()

  val config = Config()
    .setNumberOfInputs(15)
    .setOptimizationInterval(5)

  val eval = Evaluator.makeEvaluator(spec, Implementations.z3, config)
  println(eval.step(Map(x -> 10)))
  println(eval.step(Map(x -> range(13, 15))))
  println(eval.step(Map(x -> unknownReal)))


}
