package com.iravid.irwst

import org.scalactic.TypeCheckedTripleEquals
import org.scalactic.anyvals.{ PosInt, PosZDouble, PosZInt }
import org.scalatest._
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait Spec extends WordSpec with Matchers with TypeCheckedTripleEquals with GeneratorDrivenPropertyChecks {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = PosInt(50),
      maxDiscardedFactor = PosZDouble(5.0),
      minSize = PosZInt(0),
      sizeRange = PosZInt(10),
      workers = PosInt(2)
    )
}

class IRWSSpec extends Spec {
  "IRWS" must {
    "support basic usage" in {
      forAll { (initState: Int, i: Int, env: String, initLog: Vector[String], log: String) =>
        import IRWS._

        val comp = for {
          e <- ask[String, Int, Vector[String]]
          _ <- tell(Vector(log))
          s <- get
          _ <- set(s + i)
        } yield (e, s)

        val (state, resultingLog, (resultingEnv, result)) = Interpreter.runOptimized(env, initState, initLog)(comp)(_ ++ _)

        state should ===(initState + i)
        resultingLog should ===(initLog ++ Vector(log))
        resultingEnv should ===(env)
        result should === (initState)
      }
    }

    "do a noop on modify with identity" in {
      forAll { (env: String, initState: Int) =>
        import IRWS._

        val comp = modify[String, Int, Int, Unit](identity)

        Interpreter.runOptimized(env, initState, ())(comp)((_, _) => ())._1 should ===(initState)
      }
    }

    "yield the state on modify with identity" in {
      forAll { (env: String, initState: Int) =>
        import IRWS._

        val comp = inspect[String, Int, Unit, Int](identity)

        Interpreter.runOptimized(env, initState, ())(comp)((_, _) => ())._3 should ===(initState)
      }
    }
  }
}
